package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data.{toData, fromData}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.*
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

class OneWaySwapCloseSwapBenchmarkTest extends AnyFunSuite with OneWaySwapTestBase {
    // Close multiple swaps for the same trading pair.
    // n+1 should fail, n should succeed.
    test("Bench Test 1: Close Multiple Swaps (same pair)") {
        val provider = this.provider.snapshot()

        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val n = 60

        // Create enough swap UTxOs (in batches of 25)
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
                .complete(provider, sponsor = Alice.address)
                .await()
                .sign(Alice.signer)
                .transaction

            provider.submit(batchTx).await()
        }

        val allSwapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))

        // n+1 should fail
        val failUtxos = allSwapUtxos.take(n + 1)
        val failBuilder = failUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            builder.spend(
              utxo = utxo,
              redeemer = SwapRedeemer.SpendWithMint,
              requiredSigners = Set(Alice.addrKeyHash)
            )
        }
        val failTx = failBuilder
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = Map(
                AssetName(datum.pairBeacon) -> -(n + 1).toLong,
                AssetName(datum.offerBeacon) -> -(n + 1).toLong,
                AssetName(datum.askBeacon) -> -(n + 1).toLong,
              ),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        val successUtxos = allSwapUtxos.take(n)
        val successBuilder = successUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            builder.spend(
              utxo = utxo,
              redeemer = SwapRedeemer.SpendWithMint,
              requiredSigners = Set(Alice.addrKeyHash)
            )
        }
        val successTx = successBuilder
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = Map(
                AssetName(datum.pairBeacon) -> -n.toLong,
                AssetName(datum.offerBeacon) -> -n.toLong,
                AssetName(datum.askBeacon) -> -n.toLong,
              ),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        // print(fee)

        assert(provider.submit(successTx).await().isRight)
    }

    // Close multiple swap UTxOs for different trading pairs.
    // n+1 should fail, n should succeed.
    test("Bench Test 2: Close Multiple Swaps (different pairs)") {
        val provider = this.provider.snapshot()

        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val n = 60

        // Generate asset names
        val assetNames = (1 to 130).map(i => ByteString.fromString(s"TestToken$i"))
        val offerNames = assetNames.drop(65) // TestToken66..TestToken130
        val askNames = assetNames.take(65)    // TestToken1..TestToken65

        // Mint extra test tokens
        val mintTx = txBuilder
            .mint(
              assets = offerNames.map(name => AssetName(name) -> 1000L).toMap,
              redeemer = (),
              script = alwaysSucceedsMinting,
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
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
                    Coin.ada(3)
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
                .complete(provider, sponsor = Alice.address)
                .await()
                .sign(Alice.signer)
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
            val datum: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> -1L,
                  AssetName(datum.offerBeacon) -> -1L,
                  AssetName(datum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
        }
        val failTx = failBuilder
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        val successUtxos = allSwapUtxos.take(n)
        val successBuilder = successUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val datum: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> -1L,
                  AssetName(datum.offerBeacon) -> -1L,
                  AssetName(datum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
        }
        val successTx = successBuilder
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(successTx).await().isRight)
    }
}
