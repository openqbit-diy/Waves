package com.wavesplatform.utx

import java.util.concurrent.ConcurrentHashMap

import cats.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.Asset
import monix.reactive.Observer

class PessimisticPortfoliosImpl(spendableBalanceChanged: Observer[(Address, Asset)],
                                blacklistedAddressAssets: Observer[ByteStr],
                                newBlacklists: Map[Address, Portfolio] => Map[Address, Set[Asset]])
    extends PessimisticPortfolios {
  private type Portfolios = Map[Address, Portfolio]
  private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()
  private val blacklisted           = new ConcurrentHashMap[ByteStr, Set[(Address, Asset)]]()
  private val transactions          = new ConcurrentHashMap[Address, Set[ByteStr]]()

  override def add(txId: ByteStr, sender: Option[Address], txDiff: Diff): Boolean = {
    println(s"PessimisticPortfoliosImpl.add($txId, $sender, $txDiff)")
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

    val blacklists = newBlacklists(txDiff.portfolios)
    println(s"PessimisticPortfoliosImpl.newBlacklists = $blacklists")
    if (blacklists.nonEmpty) {
      val xs: Map[Address, Asset] = for {
        (addr, assets) <- blacklists
        asset          <- assets
      } yield addr -> asset

      blacklisted.put(txId, xs.toSet)
      blacklistedAddressAssets.onNext(txId)
    }

    true
  }

  override def contains(txId: ByteStr): Boolean = transactionPortfolios.containsKey(txId)

  override def isBlacklisted(accountAddr: Address, asset: Asset): Boolean = {
    val txIds = transactions.getOrDefault(accountAddr, Set.empty)
    txIds.exists { txId =>
      blacklisted.getOrDefault(txId, Set.empty).contains(accountAddr -> asset)
    }
  }

  override def getAggregated(accountAddr: Address): Portfolio = {
    val portfolios = for {
      txId <- transactions.getOrDefault(accountAddr, Set.empty).toSeq
      txPortfolios = transactionPortfolios.getOrDefault(txId, Map.empty[Address, Portfolio])
      txAccountPortfolio <- txPortfolios.get(accountAddr).toSeq
    } yield txAccountPortfolio

    Monoid.combineAll[Portfolio](portfolios)
  }

  override def remove(txId: ByteStr): Unit = {
    blacklisted.remove(txId)
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
}
