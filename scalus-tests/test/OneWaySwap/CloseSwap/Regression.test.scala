package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, fromData}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.*
import scalus.utils.await
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.{Option}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

import java.time.Instant
import cats.implicits._

class OneWaySwapCloseSwapRegressionTest extends AnyFunSuite with OneWaySwapTestBase {
    val swapAddress = genSwapAddress(Alice.addrKeyHash)

    val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
    val expirationTime = toNearestMinute(currentTime + 60_000)

    val datum1 = standardDatum()
    val datum2 = OneWaySwap.genSwapDatum(
      offerId = alwaysSucceedsMinting.scriptHash,
      offerName = utf8"TestToken2",
      askId = ByteString.empty,
      askName = ByteString.empty,
      price = Rational(1_000_000, 1)
    )
    val datum3 = OneWaySwap.genSwapDatum(
      offerId = alwaysSucceedsMinting.scriptHash,
      offerName = utf8"TestToken3",
      askId = ByteString.empty,
      askName = ByteString.empty,
      price = Rational(1_000_000, 1),
      expiration = Option.Some(expirationTime)
    )

    val createTx = txBuilder
        .references(beaconRefUtxo)
        .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
        .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
        .output(swapOutput(swapAddress, datum2, standardBeacons(datum2)))
        .output(swapOutput(swapAddress, datum3, standardBeacons(datum3)))
        .mint(
          policyId = OneWaySwap.beaconScript.scriptHash,
          assets = standardBeacons(datum1) 
               |+| standardBeacons(datum1) 
               |+| standardBeacons(datum2) 
               |+| standardBeacons(datum3),
          redeemer = BeaconRedeemer.CreateOrCloseSwaps
        )
        .validTo(Instant.ofEpochMilli(expirationTime.toLong))
        .complete(provider, sponsor = Alice.address)
        .await()
        .sign(Alice.signer)
        .transaction

    provider.submit(createTx).await()

    assert(Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail())).length == 4)

    // Close a single valid Swap UTxO. The pair is (native token,ADA).
    test("Regression Test 1: Close Single Swap") {
        val provider = this.provider.snapshot()

        val result = provider.queryUtxos { u =>
            u.output.address == swapAddress && 
            u.output.value.hasAsset(ScriptHash.fromByteString(datum1.beaconId), AssetName(datum1.pairBeacon))
        }.execute().await().getOrElse(fail()).take(1)
        val swapUtxos = Utxos.utxos(result)
        assert(swapUtxos.length == 1)

        val closeBuilder = swapUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(d.pairBeacon) -> -1L,
                  AssetName(d.offerBeacon) -> -1L,
                  AssetName(d.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
        }

        val closeTx = closeBuilder
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(closeTx).await()
    }

    // Close multiple valid Swap UTxOs. All swaps are for the same trading pair.
    test("Regression Test 2: Close Multiple Swaps (Same Pair)") {
        val provider = this.provider.snapshot()

        val result = provider.queryUtxos { u =>
            u.output.address == swapAddress && 
            u.output.value.hasAsset(ScriptHash.fromByteString(datum1.beaconId), AssetName(datum1.pairBeacon))
        }.execute().await().getOrElse(fail())
        val swapUtxos = Utxos.utxos(result)
        assert(swapUtxos.length == 2)

        val closeBuilder = swapUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(d.pairBeacon) -> -1L,
                  AssetName(d.offerBeacon) -> -1L,
                  AssetName(d.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
        }

        val closeTx = closeBuilder
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(closeTx).await()
    }

    // Close multiple valid Swap UTxOs. All swaps are for unique trading pairs.
    test("Regression Test 3: Close Multiple Swaps (Different Pairs)") {
        val provider = this.provider.snapshot()

        val result = provider.queryUtxos { u =>
            u.output.address == swapAddress 
        }.execute().await().getOrElse(fail())
        val swapUtxos = Utxos.utxos(result)
        assert(swapUtxos.length == 4)


        val closeBuilder = swapUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(d.pairBeacon) -> -1L,
                  AssetName(d.offerBeacon) -> -1L,
                  AssetName(d.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
        }

        val closeTx = closeBuilder
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(closeTx).await()
    }

    // Close an expired swap.
    test("Regression Test 4: Close Expired Swap") {
        val provider = this.provider.snapshot()

        // Advance time past expiration.
        val currentSlot = provider.currentSlot.await()
        provider.setSlot(currentSlot + 70)

        val newCurrentSlot = provider.currentSlot.await()
        assert(newCurrentSlot > SlotConfig.mainnet.timeToSlot(expirationTime.toLong))

        val result = provider.queryUtxos { u =>
            u.output.address == swapAddress && 
            u.output.value.hasAsset(ScriptHash.fromByteString(datum3.beaconId), AssetName(datum3.pairBeacon))
        }.execute().await().getOrElse(fail())
        val swapUtxos = Utxos.utxos(result)
        assert(swapUtxos.length == 1)

        val closeBuilder = swapUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(d.pairBeacon) -> -1L,
                  AssetName(d.offerBeacon) -> -1L,
                  AssetName(d.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
        }

        val closeTx = closeBuilder
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(closeTx).await()
    }

}
