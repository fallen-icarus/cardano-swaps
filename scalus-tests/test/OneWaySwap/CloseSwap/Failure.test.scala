package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, fromData}
import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.*
import scalus.utils.await
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.{Option}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

class OneWaySwapCloseSwapFailureTest extends AnyFunSuite with OneWaySwapTestBase {

    val swapAddress = genSwapAddress(Alice.addrKeyHash)
    val datum = standardDatum()

    // Create a single swap UTxO, then snapshot so every test starts from this state.
    val createTx = txBuilder
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

    provider.submit(createTx).await()

    val swapUtxo = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail())).head
    val DatumOption.Inline(rawDatum) = swapUtxo.output.datumOption.getOrElse(fail()): @unchecked
    val swapDatum: SwapDatum = fromData(rawDatum)

    // When closing a single swap UTxO, withdraw the pair beacon (don't burn it).
    test("Failure Test 1") {
        val provider = this.provider.snapshot()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  // AssetName(swapDatum.pairBeacon) -> 0L,
                  AssetName(swapDatum.offerBeacon) -> -1L,
                  AssetName(swapDatum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, withdraw the offer beacon (don't burn it).
    test("Failure Test 2") {
        val provider = this.provider.snapshot()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(swapDatum.pairBeacon) -> -1L,
                  // AssetName(swapDatum.offerBeacon) -> 0L,
                  AssetName(swapDatum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, withdraw the ask beacon (don't burn it).
    test("Failure Test 3") {
        val provider = this.provider.snapshot()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(swapDatum.pairBeacon) -> -1L,
                  AssetName(swapDatum.offerBeacon) -> -1L,
                  // AssetName(swapDatum.askBeacon) -> 0L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, withdraw all beacons with SpendWithMint redeemer
    // but don't actually burn any (all counts 0).
    test("Failure Test 4") {
        val provider = this.provider.snapshot()

        assertScriptFail("Beacon script not executed as minting policy"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, use SpendWithStake redeemer but beacon script
    // is not executed as a staking script.
    test("Failure Test 5") {
        val provider = this.provider.snapshot()

        assertScriptFail("Beacon script not executed as staking script"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(swapDatum.pairBeacon) -> -1L,
                  AssetName(swapDatum.offerBeacon) -> -1L,
                  AssetName(swapDatum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, use SpendWithStake redeemer and execute beacon
    // script as a staking script (with UpdateSwaps redeemer). Should still fail.
    test("Failure Test 6") {
        val provider = this.provider.snapshot()

        assertScriptFail(""):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(Alice.addrKeyHash)
              )
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
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, mint a non-beacon asset with the minting policy.
    test("Failure Test 7") {
        val provider = this.provider.snapshot()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(utf8"other") -> 1L,
                  AssetName(swapDatum.pairBeacon) -> -1L,
                  AssetName(swapDatum.offerBeacon) -> -1L,
                  AssetName(swapDatum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When closing a single swap UTxO, the address' staking credential did not approve.
    test("Failure Test 8") {
        val provider = this.provider.snapshot()

        assertScriptFail("Staking credential did not approve"):
            txBuilder
              .references(beaconRefUtxo)
              .references(swapRefUtxo)
              .spend(
                utxo = swapUtxo,
                redeemer = SwapRedeemer.SpendWithMint,
                requiredSigners = Set(Bob.addrKeyHash)
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(swapDatum.pairBeacon) -> -1L,
                  AssetName(swapDatum.offerBeacon) -> -1L,
                  AssetName(swapDatum.askBeacon) -> -1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps,
              )
              .complete(provider, sponsor = Bob.address)
              .await()
              .sign(Bob.signer)
              .transaction
    }
}
