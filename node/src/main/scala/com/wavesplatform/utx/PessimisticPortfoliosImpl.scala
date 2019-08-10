package com.wavesplatform.utx

import java.util.concurrent.ConcurrentHashMap

import cats.Monoid
import com.google.common.cache.CacheBuilder
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observer

class PessimisticPortfoliosImpl(spendableBalanceChanged: Observer[(Address, Asset)],
                                blacklistedAddressAssets: Observer[(Address, Asset)],
                                getBlacklists: Map[Address, Portfolio] => Map[Address, Set[Asset]])
    extends PessimisticPortfolios
    with ScorexLogging {
  private type Portfolios = Map[Address, Portfolio]
  private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()

  import scala.concurrent.duration.DurationInt
  private val expiration = 1.minute
  private val blacklisted = CacheBuilder
    .newBuilder()
    .expireAfterWrite(expiration.length, expiration.unit)
    .build[ByteStr, Set[(Address, Asset)]]()

  private val blacklistedTransactions = CacheBuilder
    .newBuilder()
    .expireAfterWrite(expiration.length, expiration.unit)
    .build[Address, Set[ByteStr]]()

  private val transactions = new ConcurrentHashMap[Address, Set[ByteStr]]()

  override def add(txId: ByteStr, txDiff: Diff): Boolean = {
    val pessimisticPortfolios         = txDiff.portfolios.map { case (addr, portfolio)        => addr -> portfolio.pessimistic }
    val nonEmptyPessimisticPortfolios = pessimisticPortfolios.filterNot { case (_, portfolio) => portfolio.isEmpty }

    if (nonEmptyPessimisticPortfolios.nonEmpty &&
        Option(transactionPortfolios.put(txId, nonEmptyPessimisticPortfolios)).isEmpty) {
      nonEmptyPessimisticPortfolios.keys.foreach { address =>
        transactions.put(address, transactions.getOrDefault(address, Set.empty) + txId)
      }
    }

    // Because we need to notify about balance changes when they are applied
    pessimisticPortfolios.foreach {
      case (addr, p) => p.assetIds.foreach(assetId => spendableBalanceChanged.onNext(addr -> assetId))
    }

    val blacklists = getBlacklists(txDiff.portfolios)
    if (blacklists.isEmpty) log.debug(s"No blacklists for $txId")
    else {
      // Not an ID but (address, asset), because if there is a bad TransferTransaction, we should cancel ExchangeTransaction
      val setBuilder = Set.newBuilder[(Address, Asset)]
      for {
        (address, assets) <- blacklists
        asset             <- assets
      } {
        val p = address -> asset
        setBuilder += p
        blacklistedAddressAssets.onNext(p)
        val prevTxs = Option(blacklistedTransactions.getIfPresent(address)).getOrElse(Set.empty)
        blacklistedTransactions.put(address, prevTxs + txId)
      }

      val r = setBuilder.result()
      log.debug(s"Blacklists for $txId:\n${r.map(_.toString()).mkString("\n")}")
      blacklisted.put(txId, r)
    }

    true
  }

  override def contains(txId: ByteStr): Boolean = transactionPortfolios.containsKey(txId)

  override def isBlacklisted(accountAddr: Address, asset: Asset): Boolean = {
    val txIds = Option(blacklistedTransactions.getIfPresent(accountAddr)).getOrElse(Set.empty)
    log.debug(s"isBlacklisted($accountAddr, $asset).txIds = ${txIds.mkString(", ")}")
    val r = txIds.exists { txId =>
      // todo getOrElse
      val b = Option(blacklisted.getIfPresent(txId)).getOrElse(Set.empty)
      val x = b.contains(accountAddr -> asset)
      log.debug(s"isBlacklisted($accountAddr, $asset).exists { txId = $txId, b = $b, x = $x }")
      x
    }
    log.debug(s"isBlacklisted($accountAddr, $asset) = $r")
    r
  }

  override def getAggregated(accountAddr: Address): Portfolio = {
    val portfolios = for {
      txId <- transactions.getOrDefault(accountAddr, Set.empty).toSeq
      txPortfolios = transactionPortfolios.getOrDefault(txId, Map.empty[Address, Portfolio])
      txAccountPortfolio <- txPortfolios.get(accountAddr).toSeq
    } yield txAccountPortfolio

    Monoid.combineAll[Portfolio](portfolios)
  }

  override def remove(txId: ByteStr): Unit =
    Option(transactionPortfolios.remove(txId)) match {
      case Some(txPortfolios) =>
        txPortfolios.foreach {
          case (addr, p) =>
            transactions.computeIfPresent(addr, (_, prevTxs) => prevTxs - txId)
            p.assetIds.foreach(assetId => spendableBalanceChanged.onNext(addr -> assetId))
        }
      case None =>
    }
}
