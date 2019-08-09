package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, Portfolio}
import com.wavesplatform.transaction.Asset
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int,
                                 maxTransactionTimeBackOffset: FiniteDuration,
                                 maxTransactionTimeForwardOffset: FiniteDuration,
                                 trackingAddressAssets: TrackingAddressAssetsSettings) {
  val allowLeasedBalanceTransferUntilHeight: Int = blockVersion3AfterHeight

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )

  def activationWindowSize(height: Int): Int =
    featureCheckBlocksPeriod * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def activationWindow(height: Int): Range =
    if (height < 1) Range(0, 0)
    else {
      val ws = activationWindowSize(height)
      Range.inclusive((height - 1) / ws * ws + 1, ((height - 1) / ws + 1) * ws)
    }

  def blocksForFeatureActivation(height: Int): Int =
    blocksForFeatureActivation * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def generatingBalanceDepth(height: Int): Int =
    if (height >= generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
}

object FunctionalitySettings {
  val MAINNET = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    allowTemporaryNegativeUntil = 1479168000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 232000,
    minimalGeneratingBalanceAfter = 1479168000000L,
    allowTransactionsFromFutureUntil = 1479168000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492768800000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492768800000L,
    resetEffectiveBalancesAtHeight = 462000,
    blockVersion3AfterHeight = 795000,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = 810000,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes,
    trackingAddressAssets = TrackingAddressAssetsSettings(Seq.empty, Set.empty)
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2700,
    allowTemporaryNegativeUntil = 1477958400000L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 1478100000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492560000000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492560000000L,
    resetEffectiveBalancesAtHeight = 51500,
    blockVersion3AfterHeight = 161700,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes,
    trackingAddressAssets = TrackingAddressAssetsSettings(Seq.empty, Set.empty)
  )

  val configPath = "waves.blockchain.custom.functionality"
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(
    1460678400000L,
    1465742577614L,
    Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption,
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)
    ),
    153722867L,
    60.seconds
  )

  val TESTNET = GenesisSettings(
    1460678400000L,
    1478000000000L,
    Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa").toOption,
    List(
      GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", (Constants.UnitsInWave * Constants.TotalWaves * 0.04).toLong),
      GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong),
      GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx",
                                 (Constants.UnitsInWave * Constants.TotalWaves - Constants.UnitsInWave * Constants.TotalWaves * 0.1).toLong)
    ),
    153722867L,
    60.seconds
  )
}

case class BlockchainSettings(addressSchemeCharacter: Char, functionalitySettings: FunctionalitySettings, genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM  = Value("CUSTOM")
}

object BlockchainSettings {
  import Asset.assetReader

  implicit def setReader[T](itemReader: ValueReader[T]): ValueReader[Set[T]] =
    CollectionReaders.traversableReader[List, T](itemReader, List.canBuildFrom[T]).map(_.toSet)

  implicit val valueReader: ValueReader[BlockchainSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  // @deprecated("Use config.as[BlockchainSettings]", "0.17.0")
  def fromRootConfig(config: Config): BlockchainSettings = config.as[BlockchainSettings]("waves.blockchain")

  private[this] def fromConfig(config: Config): BlockchainSettings = {
    def trackingAddressAssets = config.as[TrackingAddressAssetsSettings]("custom.functionality.tracking-address-assets")

    val blockchainType = config.as[BlockchainType.Value]("type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET.copy(trackingAddressAssets = trackingAddressAssets), GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('W', FunctionalitySettings.MAINNET.copy(trackingAddressAssets = trackingAddressAssets), GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings](s"custom.functionality")
        val genesisSettings        = config.as[GenesisSettings](s"custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings
    )
  }
}

case class TrackingAddressAssetsSettings(blacklists: Seq[BlacklistedAddressAssetsSettings], addressWhitelist: Set[String]) {
  val blacklistActivation: Map[(String, Asset), Int] = {
    val xs = for {
      b <- blacklists
      if !addressWhitelist.contains(b.compromisedAddress)
      asset <- b.theftAssetIds
    } yield ((b.compromisedAddress, asset), b.compromisedHeight)

    xs.toMap
  }

  def blacklistedHeight(address: String, assetId: Asset): Option[Int] = blacklistActivation.get(address -> assetId)
}

case class BlacklistedAddressAssetsSettings(compromisedAddress: String, theftAssetIds: Set[Asset], compromisedHeight: Int)

// TODO should remove all data for whitelists after start
object TrackingAddressAssetsSettings {
  def newBlacklists(currHeight: Int,
                    portfolios: Map[Address, Portfolio],
                    settings: TrackingAddressAssetsSettings,
                    isBlacklisted: (Address, Asset) => Boolean): Map[Address, Set[Asset]] = {
    {
//      val r = portfolios
//        .map {
//          case (address, portfolio) =>
//            val assets: Set[Asset] = portfolio.assets.keySet.collect {
//              case asset if shouldBlock(blockchain, currHeight, sender, address, asset, settings) => asset
//            }
//            address -> assets
//        }
//        .filter { case (_, assets) => assets.nonEmpty }

      // TODO WAVES
      val blockedAssets: Set[Asset] = portfolios.flatMap {
        case (address, p) =>
          if (settings.addressWhitelist.contains(address.toString)) Set.empty
          else p.assets.collect {
            // spendings
            case (asset, diff) if diff < 0 && shouldStartBlock(currHeight, address, asset, settings) || isBlacklisted(address, asset) =>
              asset //shouldBlock(isBlacklisted, currHeight, sender, address, asset, settings) => asset
          }
      }(collection.breakOut)

      val r = portfolios
        .collect {
          case (address, p) =>
            val newBlocks: Set[Asset] = p.assets.collect {
              // receivings
              case (asset, diff) if diff > 0 && blockedAssets.contains(asset) || shouldStartBlock(currHeight, address, asset, settings) =>
                asset: Asset
            }(collection.breakOut)
            address -> newBlocks
        }
        .filter { case (_, assets) => assets.nonEmpty }

      println(s"""TrackingAddressAssetsSettings.newBlacklists$currHeight, p=$portfolios)
           |blockedAssets = $blockedAssets
           |r = $r
           |""".stripMargin)
      r
    } // Don't need to add (sender, asset) here, because portfolios contains it
  }

  def allow(isBlacklisted: (Address, Asset) => Boolean,
            currHeight: Int,
            sender: Address,
            receiver: Address,
            asset: Asset,
            settings: TrackingAddressAssetsSettings): Boolean = {
    println(s"""settings.addressWhitelist.contains(s=${sender.stringRepr}) = ${settings.addressWhitelist.contains(sender.stringRepr)}
         |settings.addressWhitelist.contains(r=${receiver.stringRepr}) = ${settings.addressWhitelist.contains(receiver.stringRepr)}
         |isBlacklisted(s=$sender, a=$asset) = ${isBlacklisted(sender, asset)}
         |isBlacklisted(r=$receiver, a=$asset) = ${isBlacklisted(receiver, asset)}
         |shouldStartBlock(currHeight, s=$sender, $asset, settings) = ${shouldStartBlock(currHeight, sender, asset, settings)}
         |shouldStartBlock(currHeight, r=$receiver, $asset, settings) = ${shouldStartBlock(currHeight, receiver, asset, settings)}
         |""".stripMargin)
    settings.addressWhitelist.intersect(Set(sender.stringRepr, receiver.stringRepr)).nonEmpty || !{
      isBlacklisted(sender, asset) ||
      shouldStartBlock(currHeight, sender, asset, settings) ||
      shouldStartBlock(currHeight, receiver, asset, settings)
    }
  }

  def shouldStartBlock(currHeight: Int, address: Address, asset: Asset, settings: TrackingAddressAssetsSettings): Boolean =
    settings.blacklistedHeight(address.stringRepr, asset).exists(currHeight >= _)
}
