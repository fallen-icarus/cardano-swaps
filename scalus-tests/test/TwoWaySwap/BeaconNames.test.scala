package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.{Rational as _, *}

class TwoWaySwapBeaconNamesTest extends AnyFunSuite:

    val testTokenSymbol: ByteString =
        ByteString.fromHex("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d")

    val testToken1Name: ByteString = ByteString.fromString("TestToken1")
    val testToken2Name: ByteString = ByteString.fromString("TestToken2")

    val adaId: ByteString = ByteString.empty
    val adaName: ByteString = ByteString.empty

    // Test 1: Reverse direction yields the SAME pair beacon name (two-way is symmetric)
    test("uniquenessTest1 - reverse direction gives same pair beacon") {
        val forward = TwoWaySwap.genPairBeaconName(testTokenSymbol, testToken1Name, adaId, adaName)
        val reverse = TwoWaySwap.genPairBeaconName(adaId, adaName, testTokenSymbol, testToken1Name)
        assert(forward == reverse)
    }

    // Test 2: Asset beacons are different than pair beacon
    test("uniquenessTest2 - pair beacon != asset beacon") {
        val pair1 = TwoWaySwap.genPairBeaconName(testTokenSymbol, testToken1Name, adaId, adaName)
        val pair2 = TwoWaySwap.genPairBeaconName(adaId, adaName, testTokenSymbol, testToken1Name)

        assert(pair1 != TwoWaySwap.genAssetBeaconName(testTokenSymbol, testToken1Name))
        assert(pair1 != TwoWaySwap.genAssetBeaconName(adaId, adaName))
        assert(pair2 != TwoWaySwap.genAssetBeaconName(testTokenSymbol, testToken1Name))
        assert(pair2 != TwoWaySwap.genAssetBeaconName(adaId, adaName))
    }
