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

import java.time.Instant

class OneWaySwapUpdateSwapRegressionTest extends AnyFunSuite with OneWaySwapTestBase {

    val swapAddress = genSwapAddress(Alice.addrKeyHash)
    val datum = standardDatum()

    // -- Snapshot: two same-pair swap UTxOs exist (covers Tests 1 and 2) --
    val createTx = txBuilder
        .references(beaconRefUtxo)
        .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
        .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
        .mint(
          policyId = OneWaySwap.beaconScript.scriptHash,
          assets = Map(
            AssetName(datum.pairBeacon) -> 2L,
            AssetName(datum.offerBeacon) -> 2L,
            AssetName(datum.askBeacon) -> 2L,
          ),
          redeemer = BeaconRedeemer.CreateOrCloseSwaps
        )
        .complete(provider, sponsor = Alice.address)
        .await()
        .sign(Alice.signer)
        .transaction

    provider.submit(createTx).await()

    // Update a single valid Swap UTxO. The pair is (native token,ADA).
    test("Regression Test 1: Update Single Swap") {
        val provider = this.provider.snapshot()

        val swapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))

        // Update just the first swap UTxO.
        val utxo = swapUtxos.head
        val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
        val d: SwapDatum = fromData(rawDatum)
        val newDatum = datum.copy(swapPrice = Rational(500_000,2))

        val updateTx = txBuilder
            .references(beaconRefUtxo)
            .references(swapRefUtxo)
            .spend(
              utxo = utxo,
              redeemer = SwapRedeemer.SpendWithStake,
              requiredSigners = Set(Alice.addrKeyHash)
            )
            .output(swapOutput(swapAddress, newDatum, standardBeacons(newDatum)))
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

        provider.submit(updateTx).await()
    }

    // Update the price of multiple swap UTxOs.
    test("Regression Test 2: Update Multiple Swaps") {
        val provider = this.provider.snapshot()

        val swapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))

        val updateBuilder = swapUtxos.foldLeft(
            txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
        ) { (builder, utxo) =>
            val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
            val d: SwapDatum = fromData(rawDatum)
            val newDatum = d.copy(swapPrice = Rational(10, 1))
            builder
              .spend(
                utxo = utxo,
                redeemer = SwapRedeemer.SpendWithStake,
                requiredSigners = Set(Alice.addrKeyHash)
              )
              .output(swapOutput(swapAddress, newDatum, standardBeacons(newDatum)))
        }

        val updateTx = updateBuilder
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

        provider.submit(updateTx).await()
    }

    // Convert a swap UTxO to a different trading pair.
    test("Regression Test 3: Convert to Different Trading Pair") {
        val provider = this.provider.snapshot()

        // Pair 2: TestToken2 / ADA
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 1)
        )

        val swapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))
        val swapUtxo = swapUtxos.head

        // Convert to pair 2: burn old beacons, mint new beacons.
        val convertTx = txBuilder
            .references(beaconRefUtxo)
            .references(swapRefUtxo)
            .spend(
              utxo = swapUtxo,
              redeemer = SwapRedeemer.SpendWithMint,
              requiredSigners = Set(Alice.addrKeyHash)
            )
            .output(
              TransactionOutput(
                address = swapAddress,
                inlineDatum = datum2.toData,
                value = Value.assets(
                  Map(OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum2)),
                  Coin.ada(3)
                )
              )
            )
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              // Combine burns and mints, summing quantities for overlapping beacon names
              // (e.g. the ask beacon is identical when both pairs use ADA as the ask asset).
              assets = (
                Seq(
                  AssetName(datum.pairBeacon) -> -1L,
                  AssetName(datum.offerBeacon) -> -1L,
                  AssetName(datum.askBeacon) -> -1L,
                ) ++ Seq(
                  AssetName(datum2.pairBeacon) -> 1L,
                  AssetName(datum2.offerBeacon) -> 1L,
                  AssetName(datum2.askBeacon) -> 1L,
                )
              ).groupMapReduce(_._1)(_._2)(_ + _).filter(_._2 != 0L),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        provider.submit(convertTx).await()
    }

    // Move a swap to a new swap address (different staking credential).
    test("Regression Test 4: Move Swap to New Address") {
        val provider = this.provider.snapshot()

        val swapAddress2 = Address(
          env.network,
          Credential.ScriptHash(OneWaySwap.swapScriptHash),
          Credential.KeyHash(Bob.addrKeyHash)
        )

        val swapUtxos = Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail()))
        val swapUtxo = swapUtxos.head

        // Move to address 2 with updated price.
        val newDatum = datum.copy(swapPrice = Rational(10, 1))
        val updateTx = txBuilder
            .references(beaconRefUtxo)
            .references(swapRefUtxo)
            .spend(
              utxo = swapUtxo,
              redeemer = SwapRedeemer.SpendWithStake,
              requiredSigners = Set(Alice.addrKeyHash)
            )
            .output(swapOutput(swapAddress2, newDatum, standardBeacons(newDatum)))
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

        provider.submit(updateTx).await()
    }

}
