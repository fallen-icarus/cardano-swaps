package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.compiler.compile
import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.*
import scalus.testing.kit.{Party, ScalusTest}
import scalus.utils.await
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.{Option}

import scala.collection.immutable.SortedMap
import scala.util.{Try, Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

import java.time.Instant

trait OneWaySwapTestBase extends ScalusTest {
    given env: CardanoInfo = CardanoInfo.mainnet

    val alwaysSucceedsMinting = Script.PlutusV2(
      compile { (_: Data, _: Data) => () }.toUplc().plutusV2.cborByteString
    )

    val alwaysSucceedsSpending = Script.PlutusV2(
      compile { (_: Data, _: Data, _: Data) => () }.toUplc().plutusV2.cborByteString
    )

    val allUsers = Seq(
        Alice,
        Bob,
        Charles,
        Dave,
        Eve,
        Faith,
        Grace,
        Hal,
        Ivan,
        Judy,
        Kevin,
        Mallory,
        Nick,
        Oracle,
        Peggy,
        Sybil,
        Trent,
        Victor,
        Wendy,
    )

    // A mutable variable so it can be restored from snapshots.
    val provider = Emulator.withAddresses(allUsers.map(_.address))

    def mintTestTokens(user: Party) = {
        val tx = txBuilder
          .mint(
            assets = Map(
              AssetName(utf8"TestToken1") -> 1_000_000L,
              AssetName(utf8"TestToken2") -> 1_000_000L,
              AssetName(utf8"TestToken3") -> 1_000_000L,
              AssetName(utf8"TestToken4") -> 1_000_000L,
            ),
            redeemer = (),
            script = alwaysSucceedsMinting,
          )
          .complete(provider, sponsor = user.address)
          .await()
          .sign(user.signer)
          .transaction

        provider.submit(tx).await()
    }
    allUsers.map(mintTestTokens)

    /** Store the beacon script as a reference script and return the reference Utxo. */
    val beaconRefUtxo: Utxo = {
        val beaconScriptOutput = TransactionOutput(
            address = Address(
              env.network,
              Credential.ScriptHash(alwaysSucceedsSpending.scriptHash)
            ),
            value = Value.ada(0),
            datumOption = None,
            scriptRef = Some(ScriptRef(OneWaySwap.beaconScript))
        )

        val params = CardanoInfo.mainnet.protocolParams
        val beaconScriptSize = Sized(beaconScriptOutput)
        assert(MinCoinSizedTransactionOutput.ensureMinAda(beaconScriptSize, params) == Coin(20_696_620))

        val refTx = txBuilder
            .output(beaconScriptOutput)
            .registerStake(
              stakeAddress = StakeAddress(
                env.network,
                StakePayload.Script(OneWaySwap.beaconScript.scriptHash)
              ),
              witness = TwoArgumentPlutusScriptWitness.attached(
                script = OneWaySwap.beaconScript,
                redeemer = BeaconRedeemer.RegisterBeaconScript.toData
              )
            )
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        assert(provider.submit(refTx).await().isRight)

        val refInput = Input(refTx.id, 0)
        val refOutput = refTx.utxos(refInput)
        Utxo(refInput, refOutput)
    }

    /** Store the swap script as a reference script and return the reference Utxo. */
    val swapRefUtxo: Utxo = {
        val swapScriptOutput = TransactionOutput(
            address = Address(
              env.network,
              Credential.ScriptHash(alwaysSucceedsSpending.scriptHash)
            ),
            value = Value.ada(0),
            datumOption = None,
            scriptRef = Some(ScriptRef(OneWaySwap.swapScript))
        )

        val params = CardanoInfo.mainnet.protocolParams
        val swapScriptSize = Sized(swapScriptOutput)
        assert(MinCoinSizedTransactionOutput.ensureMinAda(swapScriptSize, params) == Coin(20_308_720))

        val refTx = txBuilder
            .output(swapScriptOutput)
            .complete(provider, sponsor = Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        assert(provider.submit(refTx).await().isRight)

        val refInput = Input(refTx.id, 0)
        val refOutput = refTx.utxos(refInput)
        Utxo(refInput, refOutput)
    }

    /** Create a swap address: swapScript as payment credential, seller's pub key as staking credential. */
    def genSwapAddress(sellerKeyHash: ByteString): Address =
        Address(
            env.network,
            Credential.ScriptHash(OneWaySwap.swapScriptHash),
            Credential.KeyHash(Alice.addrKeyHash)
        )

    /** Standard datum for (TestToken1, ADA) pair. */
    def standardDatum() = OneWaySwap.genSwapDatum(
      offerId = alwaysSucceedsMinting.scriptHash,
      offerName = utf8"TestToken1",
      askId = ByteString.empty,
      askName = ByteString.empty,
      price = Rational(1_000_000, 2)
    )

    /** Standard swap output builder. */
    def swapOutput(
      address: Address,
      datum: SwapDatum,
      beacons: Map[AssetName, Long],
      extraAssets: Map[ScriptHash, Map[AssetName, Long]] = Map.empty
    ) = {

        TransactionOutput(
          address = address,
          inlineDatum = datum.toData,
          value = Value.assets(
            Map(
              ScriptHash.fromByteString(datum.offerId) -> Map(AssetName(datum.offerName) -> 10L),
              OneWaySwap.beaconScript.scriptHash -> beacons,
            ) ++ extraAssets,
            Coin.ada(3)
          )
        )
    }

    /** Standard beacons map from a datum. */
    def standardBeacons(datum: SwapDatum) = Map(
      AssetName(datum.pairBeacon) -> 1L,
      AssetName(datum.offerBeacon) -> 1L,
      AssetName(datum.askBeacon) -> 1L,
    )

}
