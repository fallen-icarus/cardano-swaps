package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data.{toData, fromData}
import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.*
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

class OneWaySwapUpdateSwapBenchmarkTest extends AnyFunSuite with OneWaySwapTestBase {

    // Update swap prices for multiple same-pair UTxOs.
    // n+1 should fail, n should succeed.
    test("Bench Test 1: Update Multiple Swaps (same pair)") {
        val provider = this.provider.snapshot()

        val seller = Alice
        val swapAddress = Address(
          env.network,
          Credential.ScriptHash(OneWaySwap.swapScriptHash),
          Credential.KeyHash(seller.addrKeyHash)
        )
        val datum = standardDatum()
        val n = 30

        // Create enough swap UTxOs (in batches of 25).
        for (_ <- 1 to 3) {
            var batchBuilder = txBuilder.references(beaconRefUtxo)
            for (_ <- 1 to 25) {
                batchBuilder = batchBuilder.output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            }
            val batchTx = batchBuilder
                .mint(
                  policyId = OneWaySwap.beaconScript.scriptHash,
                  assets = Map(
                    AssetName(datum.pairBeacon) -> 25L,
                    AssetName(datum.offerBeacon) -> 25L,
                    AssetName(datum.askBeacon) -> 25L,
                  ),
                  redeemer = BeaconRedeemer.CreateOrCloseSwaps
                )
                .complete(provider, sponsor = seller.address)
                .await()
                .sign(seller.signer)
                .transaction

            provider.submit(batchTx).await()
        }

        val allSwapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))

        // n+1 should fail
        val failUtxos = allSwapUtxos.take(n + 1)
        val failBuilder = failUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            val newDatum = d.copy(swapPrice = Rational(10, 1))
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(seller.addrKeyHash)
              )
              .output(swapOutput(swapAddress, newDatum, standardBeacons(newDatum)))
        }
        val failTx = failBuilder
            .withdrawRewards(
              stakeAddress = StakeAddress(
                env.network,
                StakePayload.Script(OneWaySwap.beaconScript.scriptHash)
              ),
              amount = Coin(0),
              witness = TwoArgumentPlutusScriptWitness.reference(
                redeemer = BeaconRedeemer.UpdateSwaps.toData
              )
            )
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        val successUtxos = allSwapUtxos.take(n)
        val successBuilder = successUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            val newDatum = d.copy(swapPrice = Rational(10, 1))
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(seller.addrKeyHash)
              )
              .output(swapOutput(swapAddress, newDatum, standardBeacons(newDatum)))
        }
        val successTx = successBuilder
            .withdrawRewards(
              stakeAddress = StakeAddress(
                env.network,
                StakePayload.Script(OneWaySwap.beaconScript.scriptHash)
              ),
              amount = Coin(0),
              witness = TwoArgumentPlutusScriptWitness.reference(
                redeemer = BeaconRedeemer.UpdateSwaps.toData
              )
            )
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction

        assert(provider.submit(successTx).await().isRight)
    }

    // Update swap prices for multiple different-pair UTxOs.
    // n+1 should fail, n should succeed.
    test("Bench Test 2: Update Multiple Swaps (different pairs)") {
        val provider = this.provider.snapshot()

        val seller = Bob
        val swapAddress = Address(
          env.network,
          Credential.ScriptHash(OneWaySwap.swapScriptHash),
          Credential.KeyHash(seller.addrKeyHash)
        )
        val n = 20

        // Generate asset names
        val assetNames = (1 to 120).map(i => ByteString.fromString(s"TestToken$i"))
        val offerNames = assetNames.drop(60) // TestToken61..TestToken120
        val askNames = assetNames.take(60)   // TestToken1..TestToken60

        // Mint the offer tokens
        val mintTx = txBuilder
            .mint(
              assets = offerNames.map(name => AssetName(name) -> 1000L).toMap,
              redeemer = (),
              script = alwaysSucceedsMinting,
            )
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction
        provider.submit(mintTx).await()

        // Generate datums for n+1 pairs
        val allDatums = (0 to n).map { i =>
            OneWaySwap.genSwapDatum(
              offerId = alwaysSucceedsMinting.scriptHash,
              offerName = offerNames(i),
              askId = alwaysSucceedsMinting.scriptHash,
              askName = askNames(i),
              price = Rational(1, 1)
            )
        }

        // Create swap UTxOs in batches of 20
        for (batch <- allDatums.grouped(20).toSeq) {
            val outputs = batch.map { datum =>
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(datum.offerName) -> 10L),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum),
                    ),
                    Coin.ada(4)
                  )
                )
            }
            val beaconMints = batch
                .flatMap(d => standardBeacons(d).toSeq)
                .groupMapReduce(_._1)(_._2)(_ + _)

            var batchBuilder = txBuilder.references(beaconRefUtxo)
            for (out <- outputs) batchBuilder = batchBuilder.output(out)

            val batchTx = batchBuilder
                .mint(
                  policyId = OneWaySwap.beaconScript.scriptHash,
                  assets = beaconMints,
                  redeemer = BeaconRedeemer.CreateOrCloseSwaps
                )
                .complete(provider, sponsor = seller.address)
                .await()
                .sign(seller.signer)
                .transaction

            provider.submit(batchTx).await()
        }

        val allSwapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))

        // n+1 should fail
        val failUtxos = allSwapUtxos.take(n + 1)
        val failBuilder = failUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            val newDatum = d.copy(swapPrice = Rational(10, 1))
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(seller.addrKeyHash)
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = newDatum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(newDatum.offerName) -> 10L),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(newDatum),
                    ),
                    Coin.ada(4)
                  )
                )
              )
        }
        val failTx = failBuilder
            .withdrawRewards(
              stakeAddress = StakeAddress(
                env.network,
                StakePayload.Script(OneWaySwap.beaconScript.scriptHash)
              ),
              amount = Coin(0),
              witness = TwoArgumentPlutusScriptWitness.reference(
                redeemer = BeaconRedeemer.UpdateSwaps.toData
              )
            )
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        val successUtxos = allSwapUtxos.take(n)
        val successBuilder = successUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            val newDatum = d.copy(swapPrice = Rational(10, 1))
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(seller.addrKeyHash)
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = newDatum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(newDatum.offerName) -> 10L),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(newDatum),
                    ),
                    Coin.ada(4)
                  )
                )
              )
        }
        val successTx = successBuilder
            .withdrawRewards(
              stakeAddress = StakeAddress(
                env.network,
                StakePayload.Script(OneWaySwap.beaconScript.scriptHash)
              ),
              amount = Coin(0),
              witness = TwoArgumentPlutusScriptWitness.reference(
                redeemer = BeaconRedeemer.UpdateSwaps.toData
              )
            )
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction

        assert(provider.submit(successTx).await().isRight)
    }

    // Convert swaps to different trading pairs.
    // n+1 should fail, n should succeed.
    test("Bench Test 3: Convert Swaps to Different Pairs") {
        val provider = this.provider.snapshot()

        val seller = Charles
        val swapAddress = Address(
          env.network,
          Credential.ScriptHash(OneWaySwap.swapScriptHash),
          Credential.KeyHash(seller.addrKeyHash)
        )
        val n = 18

        // Generate asset names
        val assetNames = (1 to 120).map(i => ByteString.fromString(s"TestToken$i"))
        val offerNames = assetNames.drop(60) // TestToken61..TestToken120
        val askNames = assetNames.take(60)   // TestToken1..TestToken60

        // Mint the offer tokens
        val mintTx = txBuilder
            .mint(
              assets = offerNames.map(name => AssetName(name) -> 1000L).toMap,
              redeemer = (),
              script = alwaysSucceedsMinting,
            )
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction
        provider.submit(mintTx).await()

        // Generate 60 datums, split into before (first 40) and after (last 20)
        val allDatums = (0 until 60).map { i =>
            OneWaySwap.genSwapDatum(
              offerId = alwaysSucceedsMinting.scriptHash,
              offerName = offerNames(i),
              askId = alwaysSucceedsMinting.scriptHash,
              askName = askNames(i),
              price = Rational(1, 1)
            )
        }
        val beforeDatums = allDatums.take(40)
        val afterDatums = allDatums.drop(40) // 20 datums

        // Create swap UTxOs for beforeDatums in batches of 20
        for (batch <- beforeDatums.grouped(20).toSeq) {
            val outputs = batch.map { datum =>
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(datum.offerName) -> 10L),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum),
                    ),
                    Coin.ada(4)
                  )
                )
            }
            val beaconMints = batch
                .flatMap(d => standardBeacons(d).toSeq)
                .groupMapReduce(_._1)(_._2)(_ + _)

            var batchBuilder = txBuilder.references(beaconRefUtxo)
            for (out <- outputs) batchBuilder = batchBuilder.output(out)

            val batchTx = batchBuilder
                .mint(
                  policyId = OneWaySwap.beaconScript.scriptHash,
                  assets = beaconMints,
                  redeemer = BeaconRedeemer.CreateOrCloseSwaps
                )
                .complete(provider, sponsor = seller.address)
                .await()
                .sign(seller.signer)
                .transaction

            provider.submit(batchTx).await()
        }

        val allSwapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))

        // n+1 should fail
        val failUtxos = allSwapUtxos.take(n + 1)
        val failBuilder = failUtxos.zip(afterDatums).foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { case (builder, (utxo, afterDatum)) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val beforeDatum: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(seller.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(beforeDatum.pairBeacon) -> -1L,
                  AssetName(beforeDatum.offerBeacon) -> -1L,
                  AssetName(beforeDatum.askBeacon) -> -1L,
                  AssetName(afterDatum.pairBeacon) -> 1L,
                  AssetName(afterDatum.offerBeacon) -> 1L,
                  AssetName(afterDatum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = afterDatum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(afterDatum.offerName) -> 10L),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(afterDatum),
                    ),
                    Coin.ada(4)
                  )
                )
              )
        }
        val failTx = failBuilder
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        val successUtxos = allSwapUtxos.take(n)
        val successBuilder = successUtxos.zip(afterDatums).foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { case (builder, (utxo, afterDatum)) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val beforeDatum: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(seller.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(beforeDatum.pairBeacon) -> -1L,
                  AssetName(beforeDatum.offerBeacon) -> -1L,
                  AssetName(beforeDatum.askBeacon) -> -1L,
                  AssetName(afterDatum.pairBeacon) -> 1L,
                  AssetName(afterDatum.offerBeacon) -> 1L,
                  AssetName(afterDatum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = afterDatum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(afterDatum.offerName) -> 10L),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(afterDatum),
                    ),
                    Coin.ada(4)
                  )
                )
              )
        }
        val successTx = successBuilder
            .complete(provider, sponsor = seller.address)
            .await()
            .sign(seller.signer)
            .transaction

        assert(provider.submit(successTx).await().isRight)
    }
}
