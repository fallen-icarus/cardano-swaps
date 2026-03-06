package cardanoswaps

import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{ToData, toData}
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.Script
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.cardano.onchain.plutus.prelude.{Rational as _, *}
import scalus.uplc.Program

import java.nio.file.{Files, Path}
import java.security.MessageDigest

// -- On-Chain Data Types --

case class TwoWaySwapDatum(
    beaconId: ByteString,
    pairBeacon: ByteString,
    asset1Id: ByteString,
    asset1Name: ByteString,
    asset1Beacon: ByteString,
    asset2Id: ByteString,
    asset2Name: ByteString,
    asset2Beacon: ByteString,
    asset1Price: Rational,
    asset2Price: Rational,
    prevInput: Option[TxOutRef],
    expiration: Option[BigInt]
) derives ToData

enum TwoWaySwapRedeemer derives ToData:
    case SpendWithMint, SpendWithStake, TakeAsset1, TakeAsset2

enum TwoWayBeaconRedeemer derives ToData:
    case RegisterBeaconScript, CreateOrCloseSwaps, UpdateSwaps

// -- SDK --

object TwoWaySwap:
    private val blueprintJson = Files.readString(java.nio.file.Path.of("aiken/plutus.json"))
    private val blueprint = Blueprint.fromJson(blueprintJson)

    // Load swap script
    private val rawSwapProgram =
        blueprint.validators
            .find(_.title == "two_way_swap.swap_script")
            .get
            .compiledCode
            .map(Program.fromCborHex)
            .get

    val swapScript = Script.PlutusV2(rawSwapProgram.cborByteString)
    val swapScriptHash = swapScript.scriptHash

    // Load beacon script (parameterized by swap validator hash)
    private val rawBeaconProgram =
        blueprint.validators
            .find(_.title == "two_way_swap.beacon_script")
            .get
            .compiledCode
            .map(Program.fromCborHex)
            .get

    private val appliedBeaconProgram = rawBeaconProgram $ swapScriptHash.toData
    val beaconScript = Script.PlutusV2(appliedBeaconProgram.cborByteString)
    val beaconCurrencySymbol = beaconScript.scriptHash

    // Unapplied beacon script (for hash verification against blueprint)
    val rawBeaconScript: Script = Script.PlutusV2(rawBeaconProgram.cborByteString)
    val rawBeaconScriptHash: ByteString = rawBeaconScript.scriptHash

    // -- Beacon Name Generation --

    private def sha256(data: Array[Byte]): ByteString =
        val digest = MessageDigest.getInstance("SHA-256")
        ByteString.fromArray(digest.digest(data))

    /** Compare two asset configs lexicographically (policyId first, then name).
      * Matches Haskell Ord for (CurrencySymbol, TokenName).
      */
    private def compareAssets(
        id1: ByteString, name1: ByteString,
        id2: ByteString, name2: ByteString
    ): Int =
        val c = id1.toHex.compareTo(id2.toHex)
        if c != 0 then c else name1.toHex.compareTo(name2.toHex)

    /** Sort the pair so beacon name is order-independent, then hash.
      * ADA's empty policy ID is replaced with 0x00 after sorting.
      */
    def genPairBeaconName(
        assetXId: ByteString, assetXName: ByteString,
        assetYId: ByteString, assetYName: ByteString
    ): ByteString =
        // Sort: if Y < X then swap
        val (s1Id, s1Name, s2Id, s2Name) =
            if compareAssets(assetYId, assetYName, assetXId, assetXName) < 0
            then (assetYId, assetYName, assetXId, assetXName)
            else (assetXId, assetXName, assetYId, assetYName)
        val sym1 = if s1Id.isEmpty then ByteString.fromHex("00") else s1Id
        val sym2 = if s2Id.isEmpty then ByteString.fromHex("00") else s2Id
        sha256((sym1 ++ s1Name ++ sym2 ++ s2Name).bytes)

    /** Hash assetId ++ assetName (no prefix). */
    def genAssetBeaconName(assetId: ByteString, assetName: ByteString): ByteString =
        sha256((assetId ++ assetName).bytes)

    // -- Datum Construction --

    /** Create a TwoWaySwapDatum. Assets are sorted internally so order of
      * (first, second) doesn't matter — just match prices to the correct asset.
      */
    def genSwapDatum(
        firstId: ByteString, firstName: ByteString,
        secondId: ByteString, secondName: ByteString,
        firstPrice: Rational,
        secondPrice: Rational,
        prevInput: Option[TxOutRef] = Option.None,
        expiration: Option[BigInt] = Option.None
    ): TwoWaySwapDatum =
        // Sort assets; first < second means first becomes asset1
        val firstIsSmaller = compareAssets(firstId, firstName, secondId, secondName) <= 0
        val (a1Id, a1Name, a2Id, a2Name, a1Price, a2Price) =
            if firstIsSmaller
            then (firstId, firstName, secondId, secondName, firstPrice, secondPrice)
            else (secondId, secondName, firstId, firstName, secondPrice, firstPrice)

        TwoWaySwapDatum(
            beaconId = beaconCurrencySymbol,
            pairBeacon = genPairBeaconName(a1Id, a1Name, a2Id, a2Name),
            asset1Id = a1Id,
            asset1Name = a1Name,
            asset1Beacon = genAssetBeaconName(a1Id, a1Name),
            asset2Id = a2Id,
            asset2Name = a2Name,
            asset2Beacon = genAssetBeaconName(a2Id, a2Name),
            asset1Price = a1Price,
            asset2Price = a2Price,
            prevInput = prevInput,
            expiration = expiration
        )

    /** Determine the correct swap redeemer based on which asset you're offering/asking.
      * Matches Haskell getRequiredSwapDirection.
      */
    def getRequiredSwapDirection(
        offerId: ByteString, offerName: ByteString,
        askId: ByteString, askName: ByteString
    ): TwoWaySwapRedeemer =
        if compareAssets(offerId, offerName, askId, askName) <= 0
        then TwoWaySwapRedeemer.TakeAsset1
        else TwoWaySwapRedeemer.TakeAsset2
