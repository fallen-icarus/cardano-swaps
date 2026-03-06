package cardanoswaps

import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{ToData, toData, FromData}
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.{Script, ScriptHash}
import scalus.cardano.onchain.plutus.v2.{TxOutRef, PolicyId, TokenName}
import scalus.cardano.onchain.plutus.prelude.{Option}
import scalus.uplc.Program

import java.nio.file.{Files, Path}
import java.security.MessageDigest

// -- On-Chain Data Types --

case class Rational(numerator: BigInt, denominator: BigInt) derives ToData, FromData

case class SwapDatum(
    beaconId: PolicyId,
    pairBeacon: TokenName,
    offerId: PolicyId,
    offerName: TokenName,
    offerBeacon: TokenName,
    askId: PolicyId,
    askName: TokenName,
    askBeacon: TokenName,
    swapPrice: Rational,
    prevInput: Option[TxOutRef],
    expiration: Option[BigInt]
) derives ToData, FromData

enum SwapRedeemer derives ToData:
    case SpendWithMint, SpendWithStake, Swap

enum BeaconRedeemer derives ToData:
    case RegisterBeaconScript, CreateOrCloseSwaps, UpdateSwaps

/** Round a POSIXTime (in milliseconds) to the nearest minute. */
def toNearestMinute(time: BigInt): BigInt =
    val remainder = time % 60000
    if remainder >= 30000 then time + (60000 - remainder)
    else time - remainder

// -- SDK --

object OneWaySwap:
    private val blueprintJson = Files.readString(Path.of("aiken/plutus.json"))
    private val blueprint = Blueprint.fromJson(blueprintJson)

    // Load swap script
    private val rawSwapProgram =
        blueprint.validators
            .find(_.title == "one_way_swap.swap_script")
            .get
            .compiledCode
            .map(Program.fromCborHex)
            .get

    val swapScript = Script.PlutusV2(rawSwapProgram.cborByteString)
    val swapScriptHash = swapScript.scriptHash

    // Load beacon script (parameterized by swap validator hash)
    private val rawBeaconProgram =
        blueprint.validators
            .find(_.title == "one_way_swap.beacon_script")
            .get
            .compiledCode
            .map(Program.fromCborHex)
            .get

    private val appliedBeaconProgram = rawBeaconProgram $ swapScriptHash.toData
    val beaconScript = Script.PlutusV2(appliedBeaconProgram.cborByteString)
    val beaconCurrencySymbol = beaconScript.scriptHash

    // Unapplied beacon script (for hash verification against blueprint)
    val rawBeaconScript: Script = Script.PlutusV2(rawBeaconProgram.cborByteString)
    val rawBeaconScriptHash: ScriptHash = rawBeaconScript.scriptHash

    // -- Beacon Name Generation --

    private def sha256(data: Array[Byte]): ByteString =
        val digest = MessageDigest.getInstance("SHA-256")
        ByteString.fromArray(digest.digest(data))

    def genPairBeaconName(
        offerId: PolicyId,
        offerName: TokenName,
        askId: PolicyId,
        askName: TokenName
    ): TokenName =
        val sym1 = if offerId.isEmpty then ByteString.fromHex("00") else offerId
        val sym2 = if askId.isEmpty then ByteString.fromHex("00") else askId
        sha256((sym1 ++ offerName ++ sym2 ++ askName).bytes)

    def genOfferBeaconName(offerId: PolicyId, offerName: TokenName): TokenName =
        sha256((ByteString.fromHex("01") ++ offerId ++ offerName).bytes)

    def genAskBeaconName(askId: PolicyId, askName: TokenName): TokenName =
        sha256((ByteString.fromHex("02") ++ askId ++ askName).bytes)

    // -- Datum Construction --

    def genSwapDatum(
        offerId: PolicyId,
        offerName: TokenName,
        askId: PolicyId,
        askName: TokenName,
        price: Rational,
        prevInput: Option[TxOutRef] = Option.None,
        expiration: Option[BigInt] = Option.None
    ): SwapDatum =
        SwapDatum(
            beaconId = beaconCurrencySymbol,
            pairBeacon = genPairBeaconName(offerId, offerName, askId, askName),
            offerId = offerId,
            offerName = offerName,
            offerBeacon = genOfferBeaconName(offerId, offerName),
            askId = askId,
            askName = askName,
            askBeacon = genAskBeaconName(askId, askName),
            swapPrice = price,
            prevInput = prevInput,
            expiration = expiration
        )
