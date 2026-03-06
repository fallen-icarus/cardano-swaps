package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.toData
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

class OneWaySwapCreateSwapRegressionTest extends AnyFunSuite with OneWaySwapTestBase {

    // Create a single valid Swap UTxO. The pair is (native token,ADA).
    test("Regression Test 1: Create Single Swap") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }

    // Create multiple valid Swap UTxOs. All swaps are for the same trading pair.
    test("Regression Test 2: Create Multiple Swaps (Same Pair)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum) |+| standardBeacons(datum),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }

    // Create multiple valid Swap UTxOs. All swaps are for unique trading pairs.
    test("Regression Test 3: Create Multiple Swaps (Different Pairs)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum1 = standardDatum()

        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = alwaysSucceedsMinting.scriptHash,
          askName = utf8"TestToken3",
          price = Rational(1, 2)
        )

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
            .output(swapOutput(swapAddress, datum2, standardBeacons(datum2)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum1) |+| standardBeacons(datum2),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }

    // Another policy id mints a token in the same tx. This checks the beacon policy correctly
    // ignores other policies minting/burning in the same tx.
    test("Regression Test 4: Ignore Other Policies") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .mint(
              assets = Map(
                AssetName(utf8"TestToken1") -> 1_000_000L,
              ),
              redeemer = (),
              script = alwaysSucceedsMinting,
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }

    // Create a single valid Swap UTxO. The swap expires after 1 min.
    test("Regression Test 5: Expiring Single Swap (1-min)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
        val expirationTime = toNearestMinute(currentTime + 60_000)

        val datum = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2),
          expiration = Option.Some(expirationTime)
        )

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .validTo(SlotConfig.mainnet.slotToInstant(SlotConfig.mainnet.timeToSlot(expirationTime.toLong)))
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }

    // Create a single valid Swap UTxO. The swap expires after 10 min.
    test("Regression Test 6: Expiring Single Swap (10-min)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
        val expirationTime = toNearestMinute(currentTime + 600_000)

        val datum = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2),
          expiration = Option.Some(expirationTime)
        )

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .validTo(Instant.ofEpochMilli(expirationTime.toLong))
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }

    // Create multiple valid Swap UTxOs. All swaps are for the same trading pair, but have different
    // expirations.
    test("Regression Test 7: Create Multiple Expiring Swaps (Different Expirations)") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
        val expirationTime1 = toNearestMinute(currentTime + 60_000)
        val expirationTime2 = toNearestMinute(currentTime + 60_000)

        val datum1 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2),
          expiration = Option.Some(expirationTime1)
        )

        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2),
          expiration = Option.Some(expirationTime2)
        )

        val tx = txBuilder
            .references(beaconRefUtxo)
            .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
            .output(swapOutput(swapAddress, datum2, standardBeacons(datum2)))
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum1) |+| standardBeacons(datum2),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .validTo(Instant.ofEpochMilli(expirationTime1.toLong))
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(tx).await()
    }
}
