package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.http.DebugApiRoute.TrackedAssetsAccount
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{DebugBalanceDetails, Transaction}
import com.wavesplatform.it.sync.TrackingAddressAssetsSuite._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NodeConfigs, ReportingTestName, WaitForHeight2}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV2}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV2}
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt

class TrackingAddressAssetsSuite
    extends FreeSpec
    with Matchers
    with WaitForHeight2
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker {

  val estimator: ScriptEstimatorV2.type = ScriptEstimatorV2

  private def miner = nodes.head
  override protected def nodeConfigs: Seq[Config] =
    Seq(NodeConfigs.Default.head)
      .map(ConfigFactory.parseString("waves.miner.quorum = 0").withFallback)
      .map(TrackingAddressAssetsSuite.config.withFallback)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val assets = Seq(IssueUsdTx, IssueEthTx)

    log.info(s"""Participants:
                |${allGuys.map { case (label, keyPair) => s"${label.name}: ${keyPair.toAddress}" }.mkString("\n")}""".stripMargin)

    log.info(s"""Assets:
                |${assets.map(x => s"${new String(x.name, StandardCharsets.UTF_8)}: ${x.id()}").mkString("\n")}""".stripMargin)

    val xs = assets.map(_.json()).map(miner.broadcastRequest(_))
    xs.foreach(tx => miner.waitForTransaction(tx.id))

    val transfers = List(distributeWavesTx, distributeUsdTx, distributeEthTx)
    transfers.foreach(tx => miner.broadcastRequest(tx.json()))
    transfers.foreach(tx => miner.waitForTransaction(tx.id().toString))
  }

  def broadcastSetScript(source: KeyPair, script: Option[String], fee: Long): Transaction = {
    val tx = SetScriptTransaction
      .selfSigned(
        sender = source,
        script = script.map(x => Script.fromBase64String(x).explicitGet()),
        fee = fee,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()
    miner.broadcastRequest(tx.json())
  }

  def broadcastInvoke(
      source: KeyPair,
      dApp: String,
      func: Option[String],
      args: List[Terms.EXPR] = List.empty,
      payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
      fee: Long = 500000,
      feeAssetId: Option[String] = None,
      version: Byte = 1
  ): Transaction = {
    val tx = InvokeScriptTransaction
      .selfSigned(
        source,
        AddressOrAlias.fromString(dApp).explicitGet(),
        Some(FUNCTION_CALL(FunctionHeader.User(func.get), args)),
        payment,
        fee,
        Asset.fromString(feeAssetId),
        System.currentTimeMillis()
      )
      .explicitGet()
    miner.broadcastRequest(tx.json())
  }

  "No bad coins before transfers" in {
    allGuys.map {
      case (label, keyPair) =>
        withClue(label.name) {
          miner.debugTrackedAssets(keyPair.toAddress.stringRepr) shouldBe empty
        }
    }

    miner.debugAllTrackedAssetsByAssetId(trackedAssetStr) shouldBe empty
  }

  "Steal" in {
    miner.broadcastRequest(stealTx.json())
    miner.waitForTransaction(stealTx.id().toString)
    nodes.waitForHeightArise()

    miner.debugTrackedAssets(victim.toAddress.stringRepr) shouldBe empty

    Map(
      "badGuy1" -> badGuy1Address,
      "badGuy5" -> badGuy5Address
    ).foreach {
      case (name, address) =>
        withClue(name) {
          miner.debugTrackedAssets(address) shouldBe Seq(UsdId.toString)
          miner.debugBalanceDetails(address, UsdId.toString) shouldBe DebugBalanceDetails(
            bad = stolenUsdAmount,
            good = 0
          )
        }
    }
  }

  "Report with all stolen assets works" in {
    miner.waitForHeight(miner.height + 1) // because it doesn't consider microblocks
    miner.debugAllTrackedAssetsByAssetId(trackedAssetStr) shouldBe Map(
      badGuy1Address -> TrackedAssetsAccount(stolenUsdAmount, 0),
      badGuy5Address -> TrackedAssetsAccount(stolenUsdAmount, 0)
    )
  }

  "When all bad coins are gone, there should be no tracked assets on this address" in {
    miner.debugTrackedAssets(badGuy5Address) shouldBe Seq(UsdId.toString)

    val tx = transferUsd(badGuy5, badGuy1, stolenUsdAmount)
    miner.broadcastRequest(tx.json())
    miner.waitForTransaction(tx.id().toString)

    miner.debugTrackedAssets(badGuy5Address) shouldBe empty
    miner.debugBalanceDetails(badGuy5Address, UsdId.toString) shouldBe DebugBalanceDetails(
      bad = 0,
      good = 0
    )
  }

  "badGuy1 tries to wash bad coins from fakeMatcher with badGuy4, that still not has bad assets" in {
    val amount = 100
    val price  = 10 * Order.PriceConstant

    miner.debugTrackedAssets(badGuy1Address) shouldBe Seq(UsdId.toString)
    miner.debugTrackedAssets(badGuy4.toAddress.stringRepr) shouldBe empty
    miner.debugTrackedAssets(fakeMatcher.toAddress.stringRepr) shouldBe empty

    // Has bad USD coins
    val badGuy1Order = OrderV3.buy(
      sender = badGuy1,
      pair = ethUsdPair,
      matcher = fakeMatcher.publicKey,
      amount = amount,
      price = price,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 1.day.toMillis,
      matcherFee = 20000,
      matcherFeeAssetId = usdAsset // sends bad coins to matcher
    )

    // Has good ETH coins
    val badGuy4Order = OrderV1.sell(
      sender = badGuy4,
      pair = ethUsdPair,
      matcher = fakeMatcher.publicKey,
      amount = amount,
      price = price,
      timestamp = System.currentTimeMillis(),
      expiration = System.currentTimeMillis() + 1.day.toMillis,
      matcherFee = 300000
    )

    val exchangeTx = ExchangeTransactionV2
      .create(
        fakeMatcher,
        badGuy1Order,
        badGuy4Order,
        amount,
        price,
        badGuy1Order.matcherFee,
        badGuy4Order.matcherFee,
        300000,
        System.currentTimeMillis()
      )
      .explicitGet()

    miner.broadcastRequest(exchangeTx.json())
    miner.waitForTransaction(exchangeTx.id().toString)

    miner.debugTrackedAssets(badGuy1Address) shouldBe Seq(UsdId.toString)

    withClue("badGuy4") {
      miner.debugTrackedAssets(badGuy4.toAddress.stringRepr) shouldBe Seq(UsdId.toString)
      val receivedBadCoins = badGuy4Order.getReceiveAmount(exchangeTx.amount, exchangeTx.price).explicitGet()
      miner.debugBalanceDetails(badGuy4.toAddress.stringRepr, UsdId.toString).bad shouldBe receivedBadCoins
    }

    withClue("fakeMatcher") {
      miner.debugTrackedAssets(fakeMatcher.toAddress.stringRepr) shouldBe Seq(UsdId.toString)
      val receivedBadCoins = badGuy1Order.matcherFee
      miner.debugBalanceDetails(fakeMatcher.toAddress.stringRepr, UsdId.toString).bad shouldBe receivedBadCoins
    }
  }

  "badGuy tries to deposit or withdraw tokens using invoke script" in {

    val balanceBefore = miner.debugBalanceDetails(badGuy1Address, UsdId.toString)
    balanceBefore.bad should be > 0L
    balanceBefore.good shouldBe 0L

    val goodGuy2balanceBefore = miner.debugBalanceDetails(goodGuy2.toAddress.stringRepr, UsdId.toString)
    goodGuy2balanceBefore.bad shouldBe 0L

    val dAppScript = ScriptCompiler
      .compile(
        s"""
           |{-# STDLIB_VERSION 3 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |
           |@Callable(i)
           |func defaultDeposit() = {
           |    let pmt = extract(i.payment)
           |    WriteSet([])
           |}
           |
           |
           |@Callable(i)
           |func withdraw(amount: Int, address: String, assetId: String) = {
           |    let donationAddress = extract(addressFromString(address))
           |    TransferSet([ScriptTransfer(i.caller, amount, assetId.fromBase58String()),
           |                 ScriptTransfer(donationAddress, amount, assetId.fromBase58String())])
           |}
           |
           |
           |@Verifier(tx)
           |func verify () = true
      """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1

    val dAppSetScriptTxId = broadcastSetScript(goodGuy2, Some(dAppScript.bytes().base64), 0.015.waves).id
    miner.waitForTransaction(dAppSetScriptTxId)

    val paymentAmount = if (goodGuy2balanceBefore.good % 2 == 0) 2L else 3L

    val depositSomeBadCoins =
      broadcastInvoke(badGuy1, goodGuy2.stringRepr, Some("defaultDeposit"), List.empty, Seq(InvokeScriptTransaction.Payment(paymentAmount, usdAsset)))
    miner.waitForTransaction(depositSomeBadCoins.id)

    val goodGuy2balanceAfter = miner.debugBalanceDetails(goodGuy2.toAddress.stringRepr, UsdId.toString)
    goodGuy2balanceAfter.bad shouldBe paymentAmount

    val badGuy1balanceAfter = miner.debugBalanceDetails(badGuy1Address, UsdId.toString)
    badGuy1balanceAfter.bad shouldBe balanceBefore.bad - paymentAmount

    val withdrawSum = (goodGuy2balanceAfter.bad + goodGuy2balanceAfter.good) / 2

    val withdrawBadAsset = broadcastInvoke(
      badGuy6,
      goodGuy2.stringRepr,
      Some("withdraw"),
      args = List(CONST_LONG(withdrawSum), CONST_STRING(badGuy7.stringRepr).explicitGet(), CONST_STRING(UsdId.toString).explicitGet()),
      payment = Seq()
    )
    miner.waitForTransaction(withdrawBadAsset.id)
    nodes.waitForHeightArise()

    val badGuy6Balance = miner.debugBalanceDetails(badGuy6.toAddress.stringRepr, UsdId.toString)
    val badGuy7Balance = miner.debugBalanceDetails(badGuy7.toAddress.stringRepr, UsdId.toString)

    withClue("badGuy6") {
      badGuy6Balance.bad shouldBe 0
      badGuy6Balance.good shouldBe withdrawSum
    }

    withClue("badGuy7") {
      badGuy7Balance.good shouldBe withdrawSum - paymentAmount
      badGuy7Balance.bad shouldBe paymentAmount
    }
  }

}

object TrackingAddressAssetsSuite {

  private def privateKeyFrom(config: Config): KeyPair = KeyPair.fromSeed(config.getString("account-seed")).explicitGet()
//  val publicKey: PublicKey = PublicKey.fromBase58String(config.getString("public-key")).explicitGet()
//  val address: String      = config.getString("address")

  val alice = privateKeyFrom(NodeConfigs.Default(2))
  val bob   = privateKeyFrom(NodeConfigs.Default(3))

  val defaultAssetQuantity: Long = 999999999999L

  val usdAssetName: String         = "USD-X"
  val IssueUsdTx: IssueTransaction = mkIssue(alice, usdAssetName, defaultAssetQuantity, 2)
  val UsdId: ByteStr               = IssueUsdTx.id()
  val usd: IssuedAsset             = IssuedAsset(UsdId)

  val ethAssetName: String         = "ETH-X"
  val IssueEthTx: IssueTransaction = mkIssue(alice, ethAssetName, defaultAssetQuantity, 8)
  val EthId: ByteStr               = IssueEthTx.id()
  val eth: IssuedAsset             = IssuedAsset(EthId)

  val ethUsdPair = AssetPair(eth, usd)

  private val victim         = bob
  private val fakeMatcher    = KeyPair(ByteStr("fake matcher seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy1        = KeyPair(ByteStr("bad guy 1 seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy1Address = badGuy1.toAddress.stringRepr
  private val badGuy2        = KeyPair(ByteStr("bad guy 2 seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy3        = KeyPair(ByteStr("bad guy 3 seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy4        = KeyPair(ByteStr("bad guy 4 seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy5        = KeyPair(ByteStr("bad guy 5 seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy5Address = badGuy5.toAddress.stringRepr
  private val badGuy6        = KeyPair(ByteStr("bad guy 6 seed".getBytes(StandardCharsets.UTF_8)))
  private val badGuy7        = KeyPair(ByteStr("bad guy 7 seed".getBytes(StandardCharsets.UTF_8)))
  private val goodGuy        = alice
  private val goodGuy2       = KeyPair(ByteStr("scripted good guy seed".getBytes(StandardCharsets.UTF_8)))

  private val allGuys = Map(
    'victim      -> victim,
    'fakeMatcher -> fakeMatcher,
    'badGuy1     -> badGuy1,
    'badGuy2     -> badGuy2,
    'badGuy3     -> badGuy3,
    'badGuy4     -> badGuy4,
    'badGuy5     -> badGuy5,
    'badGuy6     -> badGuy6,
    'goodGuy     -> goodGuy,
    'goodGuy2    -> goodGuy2
  )

  private val usdAsset        = IssuedAsset(UsdId)
  private val ethAsset        = IssuedAsset(EthId)
  private val trackedAsset    = usdAsset
  private val trackedAssetStr = AssetPair.assetIdStr(usdAsset)

  private val victimUsdAmount = IssueUsdTx.quantity / allGuys.size

  private val thieves         = List(badGuy1, badGuy5)
  private val stolenUsdAmount = victimUsdAmount / (thieves.size + 1)

  private val distributeWavesTx = MassTransferTransaction
    .selfSigned(
      assetId = Waves,
      sender = goodGuy,
      transfers = (allGuys - 'goodGuy - 'victim).values.toList.map { receiver =>
        MassTransferTransaction.ParsedTransfer(receiver, 1.waves)
      },
      timestamp = System.currentTimeMillis(),
      feeAmount = 1000000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val distributeEthTx = MassTransferTransaction
    .selfSigned(
      assetId = ethAsset,
      sender = goodGuy,
      transfers = (allGuys - 'goodGuy - 'victim).values.toList.map { receiver =>
        MassTransferTransaction.ParsedTransfer(receiver, IssueEthTx.quantity / allGuys.size)
      },
      timestamp = System.currentTimeMillis(),
      feeAmount = 1000000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val distributeUsdTx = MassTransferTransaction
    .selfSigned(
      assetId = usdAsset,
      sender = goodGuy,
      transfers = List(
        MassTransferTransaction.ParsedTransfer(victim, victimUsdAmount),
        MassTransferTransaction.ParsedTransfer(badGuy3, victimUsdAmount),
        MassTransferTransaction.ParsedTransfer(goodGuy2, victimUsdAmount)
      ),
      timestamp = System.currentTimeMillis(),
      feeAmount = 1000000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val stealTx = MassTransferTransaction
    .selfSigned(
      assetId = trackedAsset,
      sender = victim,
      transfers = thieves.map { x =>
        MassTransferTransaction.ParsedTransfer(x, stolenUsdAmount)
      },
      timestamp = System.currentTimeMillis(),
      feeAmount = 1000000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val config: Config = ConfigFactory.parseString(
    s"""waves.blockchain.custom.functionality.tracking-address-assets {
       |  transfers {
       |    ${stealTx.id()} {
       |      receivers: [
       |        ${stealTx.transfers.map(x => s"""{ address: "${x.address}", asset: "${AssetPair.assetIdStr(stealTx.assetId)}" }""").mkString(", ")}
       |      ]
       |    }
       |  }
       |}""".stripMargin
  )

  private def transferUsd(from: KeyPair, to: KeyPair, amount: Long): TransferTransactionV2 = transfer(from, to, amount, usdAsset)

  private def transfer(from: KeyPair, to: KeyPair, amount: Long, assetId: Asset): TransferTransactionV2 =
    TransferTransactionV2
      .selfSigned(
        assetId = assetId,
        sender = from,
        recipient = to.toAddress,
        amount = amount,
        timestamp = System.currentTimeMillis(),
        feeAssetId = Waves,
        feeAmount = 300000,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

  private def mkIssue(
      issuer: KeyPair,
      name: String,
      quantity: Long,
      decimals: Int,
      fee: Long = issueFee,
      reissuable: Boolean = false,
      timestamp: Long = System.currentTimeMillis
  ): IssueTransaction =
    IssueTransactionV2
      .signed(
        chainId = 'I'.toByte,
        issuer,
        name.getBytes(StandardCharsets.UTF_8),
        s"$name asset".getBytes(StandardCharsets.UTF_8),
        quantity,
        decimals.toByte,
        reissuable,
        None,
        fee,
        timestamp,
        issuer
      )
      .explicitGet()
}
