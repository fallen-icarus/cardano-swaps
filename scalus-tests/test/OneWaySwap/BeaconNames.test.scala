package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class OneWaySwapBeaconNamesTest extends AnyFunSuite:

    // Arbitrary 28-byte policy ID (non-ADA)
    val testTokenSymbol: ByteString =
        ByteString.fromHex("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d")

    val testToken1Name: ByteString = ByteString.fromString("TestToken1")
    val testToken2Name: ByteString = ByteString.fromString("TestToken2")

    val adaId: ByteString = ByteString.empty
    val adaName: ByteString = ByteString.empty

    // Test 1: Reverse direction yields a different pair beacon name
    test("uniquenessTest1 - reverse direction gives different pair beacon") {
        val forward = OneWaySwap.genPairBeaconName(testTokenSymbol, testToken1Name, adaId, adaName)
        val reverse = OneWaySwap.genPairBeaconName(adaId, adaName, testTokenSymbol, testToken1Name)
        assert(forward != reverse)
    }

    // Test 2: Pair beacon is different than offer beacon
    test("uniquenessTest2 - pair beacon != offer beacon") {
        val pair1 = OneWaySwap.genPairBeaconName(testTokenSymbol, testToken1Name, adaId, adaName)
        val pair2 = OneWaySwap.genPairBeaconName(adaId, adaName, testTokenSymbol, testToken1Name)

        assert(pair1 != OneWaySwap.genOfferBeaconName(testTokenSymbol, testToken1Name))
        assert(pair1 != OneWaySwap.genOfferBeaconName(adaId, adaName))
        assert(pair2 != OneWaySwap.genOfferBeaconName(testTokenSymbol, testToken1Name))
        assert(pair2 != OneWaySwap.genOfferBeaconName(adaId, adaName))
    }

    // Test 3: Pair beacon is different than ask beacon
    test("uniquenessTest3 - pair beacon != ask beacon") {
        val pair1 = OneWaySwap.genPairBeaconName(testTokenSymbol, testToken1Name, adaId, adaName)
        val pair2 = OneWaySwap.genPairBeaconName(adaId, adaName, testTokenSymbol, testToken1Name)

        assert(pair1 != OneWaySwap.genAskBeaconName(testTokenSymbol, testToken1Name))
        assert(pair1 != OneWaySwap.genAskBeaconName(adaId, adaName))
        assert(pair2 != OneWaySwap.genAskBeaconName(testTokenSymbol, testToken1Name))
        assert(pair2 != OneWaySwap.genAskBeaconName(adaId, adaName))
    }

    // Test 4: Offer beacon is different than ask beacon
    test("uniquenessTest4 - offer beacon != ask beacon") {
        assert(
            OneWaySwap.genOfferBeaconName(testTokenSymbol, testToken1Name) !=
                OneWaySwap.genAskBeaconName(testTokenSymbol, testToken1Name)
        )
        assert(
            OneWaySwap.genOfferBeaconName(adaId, adaName) !=
                OneWaySwap.genAskBeaconName(adaId, adaName)
        )
    }

    // Test 5: Different assets have different beacons
    test("uniquenessTest5 - different assets give different beacons") {
        assert(
            OneWaySwap.genOfferBeaconName(testTokenSymbol, testToken1Name) !=
                OneWaySwap.genOfferBeaconName(adaId, adaName)
        )
        assert(
            OneWaySwap.genAskBeaconName(testTokenSymbol, testToken1Name) !=
                OneWaySwap.genAskBeaconName(adaId, adaName)
        )
        assert(
            OneWaySwap.genOfferBeaconName(testTokenSymbol, testToken1Name) !=
                OneWaySwap.genOfferBeaconName(testTokenSymbol, testToken2Name)
        )
        assert(
            OneWaySwap.genAskBeaconName(testTokenSymbol, testToken1Name) !=
                OneWaySwap.genAskBeaconName(testTokenSymbol, testToken2Name)
        )
    }
