package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{toData, fromData}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.onchain.plutus.v2.TxOutRef
import scalus.testing.kit.Party.*
import scalus.utils.await
import scalus.cardano.onchain.plutus.prelude.{Option}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

import java.time.Instant
import cats.implicits._

class OneWaySwapSwapRegressionTest extends AnyFunSuite with OneWaySwapTestBase {
    // val swapAddress = genSwapAddress(Alice.addrKeyHash)
    //
    // val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
    // val expirationTime = toNearestMinute(currentTime + 60_000)
    //
    // val datum1 = standardDatum()
    // val datum2 = OneWaySwap.genSwapDatum(
    //   offerId = alwaysSucceedsMinting.scriptHash,
    //   offerName = utf8"TestToken2",
    //   askId = ByteString.empty,
    //   askName = ByteString.empty,
    //   price = Rational(1_000_000, 1)
    // )
    // val datum3 = OneWaySwap.genSwapDatum(
    //   offerId = alwaysSucceedsMinting.scriptHash,
    //   offerName = utf8"TestToken3",
    //   askId = ByteString.empty,
    //   askName = ByteString.empty,
    //   price = Rational(1_000_000, 1),
    //   expiration = Option.Some(expirationTime)
    // )
    //
    // val createTx = txBuilder
    //     .references(beaconRefUtxo)
    //     .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
    //     .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
    //     .output(swapOutput(swapAddress, datum2, standardBeacons(datum2)))
    //     .output(swapOutput(swapAddress, datum3, standardBeacons(datum3)))
    //     .mint(
    //       policyId = OneWaySwap.beaconScript.scriptHash,
    //       assets = standardBeacons(datum1) 
    //            |+| standardBeacons(datum1) 
    //            |+| standardBeacons(datum2) 
    //            |+| standardBeacons(datum3),
    //       redeemer = BeaconRedeemer.CreateOrCloseSwaps
    //     )
    //     .validTo(Instant.ofEpochMilli(expirationTime.toLong))
    //     .complete(provider, sponsor = Alice.address)
    //     .await()
    //     .sign(Alice.signer)
    //     .transaction
    //
    // provider.submit(createTx).await()
    //
    // assert(Utxos.utxos(provider.findUtxos(swapAddress).await().getOrElse(fail())).length == 4)
    //
    // // Swap with a single valid Swap UTxO. The pair is (native token,ADA).
    // test("Regression Test 1: Swap with a Single Swap") {
    //     val provider = this.provider.snapshot()
    //
    //     val result = provider.queryUtxos { u =>
    //         u.output.address == swapAddress && 
    //         u.output.value.hasAsset(ScriptHash.fromByteString(datum1.beaconId), AssetName(datum1.pairBeacon))
    //     }.limit(1).execute().await().getOrElse(fail())
    //     val swapUtxos = Utxos.utxos(result)
    //     assert(swapUtxos.length == 1)
    //
    //     val swapBuilder = swapUtxos.foldLeft(
    //         txBuilder.references(beaconRefUtxo).references(swapRefUtxo)
    //     ) { (builder, utxo) =>
    //         val DatumOption.Inline(rawDatum) = utxo.output.datumOption.getOrElse(fail()): @unchecked
    //         val d: SwapDatum = fromData(rawDatum)
    //         val adaValue = utxo.output.value.coin
    //         builder
    //           .spend(
    //             utxo = utxo,
    //             redeemer = SwapRedeemer.Swap,
    //           )
    //           .output(
    //             TransactionOutput(
    //               address = utxo.output.address,
    //               inlineDatum = d.copy(prevInput = utxo.input).toData,
    //               value = Value.assets(
    //                 Map(
    //                   ScriptHash.fromByteString(d.offerId) -> Map(AssetName(d.offerName) -> 8L),
    //                   OneWaySwap.beaconScript.scriptHash -> standardBeacons(d),
    //                 ),
    //                 adaValue + Coin.ada(1)
    //               )
    //             )
    //           )
    //     }
    //
    //     val swapTx = swapBuilder
    //         .complete(provider, sponsor = Bob.address)
    //         .await()
    //         .sign(Bob.signer)
    //         .transaction
    //
    //     provider.submit(swapTx).await()
    // }
}
