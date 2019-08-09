package com.wavesplatform.utx

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.Asset

trait PessimisticPortfolios {
  def add(txId: ByteStr, sender: Option[Address], txDiff: Diff): Unit
  def contains(txId: ByteStr): Boolean
  def isBlacklisted(accountAddr: Address, asset: Asset): Boolean
  def getAggregated(accountAddr: Address): Portfolio
  def remove(txId: ByteStr): Unit
}
