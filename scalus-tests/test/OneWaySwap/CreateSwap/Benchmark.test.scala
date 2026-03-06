package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.*
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

class OneWaySwapCreateSwapBenchmarkTest extends AnyFunSuite with OneWaySwapTestBase {

    // Create multiple swap UTxOs for the same trading pair. The trading pair is (native asset, ADA).
    // n+1 should fail, n should succeed.
    test("Bench Test 1: Create Multiple Swaps (same pair)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val n = 34

        // n+1 should fail
        val failBuilder = (1 to (n + 1)).foldLeft(txBuilder.references(beaconRefUtxo)) { (b, _) =>
            b.output(swapOutput(swapAddress, datum, standardBeacons(datum)))
        }
        val failTx = failBuilder
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = Map(
                AssetName(datum.pairBeacon) -> (n + 1).toLong,
                AssetName(datum.offerBeacon) -> (n + 1).toLong,
                AssetName(datum.askBeacon) -> (n + 1).toLong,
              ),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        var builder = txBuilder.references(beaconRefUtxo)
        for (_ <- 1 to n) {
            builder = builder.output(swapOutput(swapAddress, datum, standardBeacons(datum)))
        }

        val tx = builder
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = Map(
                AssetName(datum.pairBeacon) -> n.toLong,
                AssetName(datum.offerBeacon) -> n.toLong,
                AssetName(datum.askBeacon) -> n.toLong,
              ),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(tx).await().isRight)
    }

    // Create multiple swap UTxOs for different trading pairs.
    // n+1 should fail, n should succeed.
    test("Bench Test 2: Create Multiple Swaps (different pairs)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val n = 25

        // Generate asset names: TestToken1..TestToken80
        val assetNames = (1 to 80).map(i => ByteString.fromString(s"TestToken$i"))
        val offerNames = assetNames.drop(40).take(n + 1) // TestToken41..TestToken65 (need n+1)
        val askNames = assetNames.take(n + 1)             // TestToken1..TestToken25 (need n+1)

        // Mint the offer tokens so they are available
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

        // n+1 should fail
        val failBuilder = allDatums.foldLeft(txBuilder.references(beaconRefUtxo)) { (b, datum) =>
            b.output(
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
            )
        }
        val failBeaconMints = allDatums
            .flatMap(d => standardBeacons(d).toSeq)
            .groupMapReduce(_._1)(_._2)(_ + _)

        val failTx = failBuilder
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = failBeaconMints,
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(failTx).await().isLeft)

        // n should succeed
        val datums = allDatums.take(n)

        var builder = txBuilder.references(beaconRefUtxo)
        for (datum <- datums) {
            builder = builder.output(
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
            )
        }

        // Compute combined beacon mints
        val beaconMints = datums
            .flatMap(d => standardBeacons(d).toSeq)
            .groupMapReduce(_._1)(_._2)(_ + _)

        val tx = builder
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = beaconMints,
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(provider.submit(tx).await().isRight)
    }
}
