package com.wavesplatform.mining

import com.wavesplatform.account.Alias
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl
import com.wavesplatform.mining.microblocks.MicroBlockMinerImpl.MicroBlockMiningResult
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.transaction.{CreateAliasTransaction, GenesisTransaction, TxVersion}
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.{TestValues, TransactionGen}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.concurrent.duration._
import scala.util.Random

class MicroBlockMinerSpec extends FlatSpec with Matchers with PrivateMethodTester with PathMockFactory with WithDomain with TransactionGen {
  "Micro block miner" should "generate microblocks in flat interval" in {
    val scheduler = Schedulers.singleThread("test")
    val acc       = TestValues.keyPair
    val genesis   = GenesisTransaction.create(acc.toAddress, TestValues.bigMoney, TestValues.timestamp).explicitGet()
    val settings  = domainSettingsWithFS(TestFunctionalitySettings.withFeatures(BlockchainFeatures.NG))
    withDomain(settings) { d =>
      d.appendBlock(TestBlock.create(Seq(genesis)))
      val utxPool = new UtxPoolImpl(ntpTime, d.blockchainUpdater, ignoreSpendableBalanceChanged, settings.utxSettings, enablePriorityPool = true)
      val microBlockMiner = new MicroBlockMinerImpl(
        _ => (),
        null,
        d.blockchainUpdater,
        utxPool,
        settings.minerSettings,
        scheduler,
        scheduler,
        Task.sleep(200 millis)
      )
      val generateOneMicroBlockTask = PrivateMethod[Task[MicroBlockMiningResult]]('generateOneMicroBlockTask)

      def generateBlocks(
          block: Block,
          constraint: MiningConstraint,
          lastMicroBlock: Long
      ): Block = {
        val task = Task.defer(microBlockMiner invokePrivate generateOneMicroBlockTask(
          acc,
          block,
          MiningConstraints(d.blockchainUpdater, d.blockchainUpdater.height, Some(settings.minerSettings)),
          constraint,
          lastMicroBlock
        ))
        import Scheduler.Implicits.global
        val startTime = System.nanoTime()
        val tx = CreateAliasTransaction
          .selfSigned(TxVersion.V1, acc, Alias.create("test" + Random.nextInt()).explicitGet(), TestValues.fee, TestValues.timestamp)
          .explicitGet()
        utxPool.putIfNew(tx).resultE shouldBe 'right
        utxPool.size should be > 0
        val result = task.runSyncUnsafe()
        result match {
          case res @ MicroBlockMinerImpl.Success(b, totalConstraint) =>
            val isFirstBlock = block.transactionData.isEmpty
            val elapsed = (res.nanoTime - startTime).nanos.toMillis

            if (isFirstBlock) elapsed should be < 1000L
            else elapsed shouldBe settings.minerSettings.microBlockInterval.toMillis +- 1000

            generateBlocks(b, totalConstraint, res.nanoTime)
          case MicroBlockMinerImpl.Stop =>
            d.blockchainUpdater.liquidBlock(d.blockchainUpdater.lastBlockId.get).get
          case MicroBlockMinerImpl.Retry =>
            throw new IllegalStateException()
        }
      }

      val baseBlock = Block
        .buildAndSign(
          3,
          TestValues.timestamp,
          d.lastBlockId,
          d.lastBlock.header.baseTarget,
          d.lastBlock.header.generationSignature,
          Nil,
          acc,
          Nil,
          0
        )
        .explicitGet()

      d.appendBlock(baseBlock)

      val constraint = OneDimensionalMiningConstraint(5, TxEstimators.one, "limit")
      val lastBlock = generateBlocks(baseBlock, constraint, 0)
      lastBlock.transactionData should have size constraint.rest.toInt
    }
    }
}