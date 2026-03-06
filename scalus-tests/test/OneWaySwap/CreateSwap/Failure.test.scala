package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.address.Address
import scalus.testing.kit.Party.*
import scalus.utils.await
import scalus.cardano.onchain.plutus.prelude.{Option}
import scalus.cardano.onchain.plutus.v1.{Value as OnchainValue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

import java.time.Instant
import cats.implicits._

class OneWaySwapCreateSwapFailureTest extends AnyFunSuite with OneWaySwapTestBase {

    // Datum has the right trading pair beacon, but the actual pair beacon minted is wrong.
    test("Failure Test 1") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum.toData,
                  value = Value.assets(
                    Map(
                        alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                        OneWaySwap.beaconScript.scriptHash -> Map(
                          AssetName(utf8"hello") -> 1L,
                          AssetName(datum.offerBeacon) -> 1L,
                          AssetName(datum.askBeacon) -> 1L,
                        ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(utf8"hello") -> 1L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // The trading pair corresponds to a different pair beacon than the one actually minted. The
    // datum also has the wrong pair beacon name.
    test("Failure Test 2") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(pairBeacon = utf8"")

        assertScriptFail("Wrong pair_beacon"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(wrongDatum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(wrongDatum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // The trading pair corresponds to a different pair beacon than the one in the datum. The proper
    // pair beacon was minted.
    test("Failure Test 3") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(pairBeacon = utf8"")

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint an additional pair beacon and withdraw it.
    test("Failure Test 4") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 2L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint an additional pair beacon and store it in the swap UTxO.
    test("Failure Test 5") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, Map(
                AssetName(datum.pairBeacon) -> 2L,
                AssetName(datum.offerBeacon) -> 1L,
                AssetName(datum.askBeacon) -> 1L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 2L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // The offer beacon minted corresponds to a different offer asset. Datum has correct offer.
    test("Failure Test 6") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongOfferBeacon = OneWaySwap.genOfferBeaconName(alwaysSucceedsMinting.scriptHash, utf8"TestToken2")

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, Map(
                AssetName(datum.pairBeacon) -> 1L,
                AssetName(wrongOfferBeacon) -> 1L,
                AssetName(datum.askBeacon) -> 1L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(wrongOfferBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Wrong offer beacon minted. Datum also has wrong offer beacon.
    test("Failure Test 7") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongOfferBeacon = OneWaySwap.genOfferBeaconName(alwaysSucceedsMinting.scriptHash, utf8"TestToken2")
        val wrongDatum = datum.copy(offerBeacon = wrongOfferBeacon)

        assertScriptFail("Wrong offer_beacon"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, Map(
                AssetName(datum.pairBeacon) -> 1L,
                AssetName(wrongOfferBeacon) -> 1L,
                AssetName(datum.askBeacon) -> 1L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(wrongOfferBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Correct offer beacon minted but datum has wrong offer beacon.
    test("Failure Test 8") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongOfferBeacon = OneWaySwap.genOfferBeaconName(alwaysSucceedsMinting.scriptHash, utf8"TestToken2")
        val wrongDatum = datum.copy(offerBeacon = wrongOfferBeacon)

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint an additional offer beacon and withdraw it.
    test("Failure Test 9") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(datum.offerBeacon) -> 2L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint an additional offer beacon and store it in the swap UTxO.
    test("Failure Test 10") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, Map(
                AssetName(datum.pairBeacon) -> 1L,
                AssetName(datum.offerBeacon) -> 2L,
                AssetName(datum.askBeacon) -> 1L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(datum.offerBeacon) -> 2L,
                  AssetName(datum.askBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Wrong ask beacon minted. Datum has correct ask.
    test("Failure Test 11") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongAskBeacon = OneWaySwap.genAskBeaconName(alwaysSucceedsMinting.scriptHash, utf8"TestToken2")

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, Map(
                AssetName(datum.pairBeacon) -> 1L,
                AssetName(datum.offerBeacon) -> 1L,
                AssetName(wrongAskBeacon) -> 1L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(wrongAskBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Wrong ask beacon minted. Datum also has wrong ask beacon.
    test("Failure Test 12") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongAskBeacon = OneWaySwap.genAskBeaconName(alwaysSucceedsMinting.scriptHash, utf8"TestToken2")
        val wrongDatum = datum.copy(askBeacon = wrongAskBeacon)

        assertScriptFail("Wrong ask_beacon"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, Map(
                AssetName(datum.pairBeacon) -> 1L,
                AssetName(datum.offerBeacon) -> 1L,
                AssetName(wrongAskBeacon) -> 1L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(wrongAskBeacon) -> 1L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Correct ask beacon minted but datum has wrong ask beacon.
    test("Failure Test 13") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongAskBeacon = OneWaySwap.genAskBeaconName(alwaysSucceedsMinting.scriptHash, utf8"TestToken2")
        val wrongDatum = datum.copy(askBeacon = wrongAskBeacon)

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint an additional ask beacon and withdraw it.
    test("Failure Test 14") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 2L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint an additional ask beacon and store it in the swap UTxO.
    test("Failure Test 15") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, Map(
                AssetName(datum.pairBeacon) -> 1L,
                AssetName(datum.offerBeacon) -> 1L,
                AssetName(datum.askBeacon) -> 2L,
              )))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum.pairBeacon) -> 1L,
                  AssetName(datum.offerBeacon) -> 1L,
                  AssetName(datum.askBeacon) -> 2L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Beacon script mints an extra unrelated token and it is withdrawn.
    test("Failure Test 16") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum) + (AssetName(utf8"other") -> 1L),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Beacon script mints an extra unrelated token and it is stored in the swap UTxO.
    test("Failure Test 17") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("One-way swaps must have exactly three kinds of beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum,
                standardBeacons(datum) + (AssetName(utf8"other") -> 1L)
              ))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum) + (AssetName(utf8"other") -> 1L),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Mint extra beacons and store them outside swap address, with proper datum.
    test("Failure Test 18") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("Beacon must go to a  DApp address with staking"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .output(
                TransactionOutput(
                  address = Alice.address,
                  inlineDatum = datum.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum)
                    ),
                    Coin.ada(3)
                  )
                )
              )
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
    }

    // Mint extra beacons and store them outside swap address, without datum.
    test("Failure Test 19") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("Beacon must go to a  DApp address with staking"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .output(
                TransactionOutput(
                  address = Alice.address,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum),
                    ),
                    Coin.ada(3)
                  )
                )
              )
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
    }

    // Mint extra beacons in a separate UTxO at swap address, without datum.
    test("Failure Test 20") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("All swap datums must be inline datums"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .output(
                TransactionOutput(
                  address = swapAddress,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum),
                    ),
                    Coin.ada(3)
                  )
                )
              )
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
    }

    // Mint extra beacons at swap address without staking credential, with proper datum.
    test("Failure Test 21") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val swapAddressNoStaking = Address(env.network, Credential.ScriptHash(OneWaySwap.swapScriptHash))
        val datum = standardDatum()

        assertScriptFail("Beacon must go to a  DApp address with staking"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .output(swapOutput(swapAddressNoStaking, datum, standardBeacons(datum)))
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
    }

    // Single valid swap at address without staking credential.
    test("Failure Test 22") {
        val provider = this.provider.snapshot()
        val swapAddressNoStaking = Address(env.network, Credential.ScriptHash(OneWaySwap.swapScriptHash))
        val datum = standardDatum()

        assertScriptFail("Beacon must go to a  DApp address with staking"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddressNoStaking, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Single valid swap stored at a non-swap address.
    test("Failure Test 23") {
        val provider = this.provider.snapshot()
        val nonSwapAddress = Address(
          env.network,
          Credential.ScriptHash(alwaysSucceedsSpending.scriptHash),
          Credential.KeyHash(Alice.addrKeyHash)
        )
        val datum = standardDatum()

        assertScriptFail("Beacon must go to a  DApp address with staking"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(nonSwapAddress, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap datum has the wrong beacon id.
    test("Failure Test 24") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(beaconId = ByteString.empty)

        assertScriptFail("Wrong beacon_id"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap datum has the wrong offer id.
    test("Failure Test 25") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(offerId = OnchainValue.adaPolicyId)

        assertScriptFail("No extraneous assets allowed in the UTxO"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap datum has the wrong offer name.
    test("Failure Test 26") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(offerName = ByteString.empty)

        assertScriptFail("Wrong pair_beacon"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap datum has the wrong ask id.
    test("Failure Test 27") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(askId = alwaysSucceedsMinting.scriptHash)

        assertScriptFail("Wrong pair_beacon"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap datum has the wrong ask name.
    test("Failure Test 28") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()
        val wrongDatum = datum.copy(askName = utf8"Other")

        assertScriptFail("Wrong pair_beacon"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Offer asset and ask asset are the same.
    test("Failure Test 29") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = alwaysSucceedsMinting.scriptHash,
          askName = utf8"TestToken1",
          price = Rational(1_000_000, 2)
        )

        assertScriptFail("Offer asset cannot be same as ask asset"):
            txBuilder
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
    }

    // Swap datum has a zero swap price numerator.
    test("Failure Test 30") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(0, 1)
        )

        assertScriptFail("swap_price numerator not > 0"):
            txBuilder
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
    }

    // Swap datum has a negative swap price numerator.
    test("Failure Test 31") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(-1, 1)
        )

        assertScriptFail("swap_price numerator not > 0"):
            txBuilder
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
    }

    // Swap datum has a zero swap price denominator.
    test("Failure Test 32") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum().copy(swapPrice = Rational(1, 0))

        assertScriptFail("swap_price denominator not > 0"):
            txBuilder
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
    }

    // Swap datum has a negative swap price denominator.
    test("Failure Test 33") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum().copy(swapPrice = Rational(1, -1))

        assertScriptFail("swap_price denominator not > 0"):
            txBuilder
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
    }

    // Swap UTxO has an extraneous asset.
    test("Failure Test 34") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("No extraneous assets allowed in the UTxO"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum),
                extraAssets = Map(
                  alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10)
                )
              ))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap datum is a datum hash instead of inline datum.
    test("Failure Test 35") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("All swap datums must be inline datums"):
          txBuilder
            .references(beaconRefUtxo)
            .output(
              TransactionOutput(
                address = swapAddress,
                value = Value.assets(
                  Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum),
                  ),
                  Coin.ada(3),
                ),
                datumHash = DataHash.fromByteString(datum.toData.dataHash),
              )
            )
            .mint(
              policyId = OneWaySwap.beaconScript.scriptHash,
              assets = standardBeacons(datum),
              redeemer = BeaconRedeemer.CreateOrCloseSwaps
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
    }

    // When creating multiple swaps for different pairs, pair beacons are mixed up.
    test("Failure Test 36") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val datum1 = standardDatum()
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum1.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> Map(
                        AssetName(datum2.pairBeacon) -> 1L, // wrong pair beacon
                        AssetName(datum1.offerBeacon) -> 1L,
                        AssetName(datum1.askBeacon) -> 1L,
                      ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum2.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> Map(
                        AssetName(datum1.pairBeacon) -> 1L, // wrong pair beacon
                        AssetName(datum2.offerBeacon) -> 1L,
                        AssetName(datum2.askBeacon) -> 1L,
                      ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum1) ++ standardBeacons(datum2),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When creating multiple swaps for different pairs, offer beacons are mixed up.
    test("Failure Test 37") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val datum1 = standardDatum()
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum1.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> Map(
                        AssetName(datum1.pairBeacon) -> 1L,
                        AssetName(datum2.offerBeacon) -> 1L, // wrong offer beacon
                        AssetName(datum1.askBeacon) -> 1L,
                      ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum2.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> Map(
                        AssetName(datum2.pairBeacon) -> 1L,
                        AssetName(datum1.offerBeacon) -> 1L, // wrong offer beacon
                        AssetName(datum2.askBeacon) -> 1L,
                      ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum1) ++ standardBeacons(datum2),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When creating multiple swaps for different pairs, ask beacons are mixed up.
    test("Failure Test 38") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val datum1 = standardDatum()
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = alwaysSucceedsMinting.scriptHash,
          askName = utf8"TestToken3",
          price = Rational(1_000_000, 2)
        )

        assertScriptFail("UTxO has wrong beacons"):
            txBuilder
              .references(beaconRefUtxo)
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum1.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> Map(
                        AssetName(datum1.pairBeacon) -> 1L,
                        AssetName(datum1.offerBeacon) -> 1L,
                        AssetName(datum2.askBeacon) -> 1L, // wrong ask beacon
                      ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum2.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> Map(
                        AssetName(datum2.pairBeacon) -> 1L,
                        AssetName(datum2.offerBeacon) -> 1L,
                        AssetName(datum1.askBeacon) -> 1L, // wrong ask beacon
                      ),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum1) ++ standardBeacons(datum2),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // When creating multiple swaps for different pairs, datums are swapped.
    test("Failure Test 39") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val datum1 = standardDatum()
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )

        assertScriptFail("No extraneous assets allowed in the UTxO"):
            txBuilder
              .references(beaconRefUtxo)
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum2.toData, // wrong datum
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum1),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum1.toData, // wrong datum
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum2),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum1) ++ standardBeacons(datum2),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Creating multiple swap outputs, the first output has wrong beacon_id.
    test("Failure Test 40") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val datum1 = standardDatum()
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )
        val datum3 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken3",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )
        val wrongDatum1 = datum1.copy(beaconId = ByteString.empty)

        assertScriptFail("Wrong beacon_id"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, wrongDatum1, standardBeacons(datum1)))
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum2.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum2),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum3.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken3") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum3),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum1) ++ standardBeacons(datum2) ++ standardBeacons(datum3),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Creating multiple swap outputs, the second output has wrong beacon_id.
    test("Failure Test 41") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)

        val datum1 = standardDatum()
        val datum2 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken2",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )
        val datum3 = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken3",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2)
        )
        val wrongDatum2 = datum2.copy(beaconId = ByteString.empty)

        assertScriptFail("Wrong beacon_id"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = wrongDatum2.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken2") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum2),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .output(
                TransactionOutput(
                  address = swapAddress,
                  inlineDatum = datum3.toData,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken3") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum3),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum1) ++ standardBeacons(datum2) ++ standardBeacons(datum3),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // New swap UTxO does not have a datum.
    test("Failure Test 42") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val datum = standardDatum()

        assertScriptFail("All swap datums must be inline datums"):
            txBuilder
              .references(beaconRefUtxo)
              .output(
                TransactionOutput(
                  address = swapAddress,
                  value = Value.assets(
                    Map(
                      alwaysSucceedsMinting.scriptHash -> Map(AssetName(utf8"TestToken1") -> 10),
                      OneWaySwap.beaconScript.scriptHash -> standardBeacons(datum),
                    ),
                    Coin.ada(3)
                  )
                )
              )
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Swap expires but invalid-hereafter flag isn't set.
    test("Failure Test 43") {
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

        assertScriptFail("invalid-hereafter required but not set"):
            txBuilder
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
    }

    // Swap expires after 30 seconds (not a 1-min interval).
    test("Failure Test 44") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
        val expirationTime = currentTime + 30_000 // 30 seconds, not rounded to minute

        val datum = OneWaySwap.genSwapDatum(
          offerId = alwaysSucceedsMinting.scriptHash,
          offerName = utf8"TestToken1",
          askId = ByteString.empty,
          askName = ByteString.empty,
          price = Rational(1_000_000, 2),
          expiration = Option.Some(expirationTime)
        )

        assertScriptFail("Must use 1-min expiration intervals"):
            txBuilder
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
    }

    // Expiration is before the invalid-hereafter.
    test("Failure Test 45") {
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

        // Set invalid-hereafter to 1 slot after expiration
        val hereafterSlot = SlotConfig.mainnet.timeToSlot(expirationTime.toLong) + 1
        val hereafterInstant = SlotConfig.mainnet.slotToInstant(hereafterSlot)

        assertScriptFail("Expiration must be >= invalid-hereafter"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum, standardBeacons(datum)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = standardBeacons(datum),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .validTo(hereafterInstant)
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

    // Two expiring swaps but invalid-hereafter is between the two expiration times.
    test("Failure Test 46") {
        val provider = this.provider.snapshot()
        val swapAddress = genSwapAddress(Alice.addrKeyHash)
        val currentTime = SlotConfig.mainnet.slotToTime(provider.currentSlot.await())
        val expirationTime1 = toNearestMinute(currentTime + 600_000)
        val expirationTime2 = expirationTime1 + 60_000

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

        // Set invalid-hereafter between the two expirations (after expiration1, before expiration2)
        val hereafterSlot = SlotConfig.mainnet.timeToSlot(expirationTime1.toLong) + 30
        val hereafterInstant = SlotConfig.mainnet.slotToInstant(hereafterSlot)

        assertScriptFail("Expiration must be >= invalid-hereafter"):
            txBuilder
              .references(beaconRefUtxo)
              .output(swapOutput(swapAddress, datum1, standardBeacons(datum1)))
              .output(swapOutput(swapAddress, datum2, standardBeacons(datum2)))
              .mint(
                policyId = OneWaySwap.beaconScript.scriptHash,
                assets = Map(
                  AssetName(datum1.pairBeacon) -> 2L,
                  AssetName(datum1.offerBeacon) -> 2L,
                  AssetName(datum1.askBeacon) -> 2L,
                ),
                redeemer = BeaconRedeemer.CreateOrCloseSwaps
              )
              .validTo(hereafterInstant)
              .complete(provider, sponsor = Alice.address)
              .await()
              .sign(Alice.signer)
              .transaction
    }

}
