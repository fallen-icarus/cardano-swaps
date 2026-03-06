package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.{Rational as _, *}

class TwoWaySwapDatumTest extends AnyFunSuite:

    val testTokenSymbol: ByteString =
        ByteString.fromHex("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d")

    test("genSwapDatum produces correct datum with sorted assets") {
        val tokenId = testTokenSymbol
        val tokenName = ByteString.fromString("TestToken1")
        val adaId = ByteString.empty
        val adaName = ByteString.empty
        val price1 = Rational(1, 2) // price to take first asset
        val price2 = Rational(3, 1) // price to take second asset

        // Pass token first, ADA second — datum should sort them
        val datum = TwoWaySwap.genSwapDatum(
            tokenId, tokenName, adaId, adaName, price1, price2
        )

        assert(datum.beaconId == TwoWaySwap.beaconCurrencySymbol)
        assert(datum.pairBeacon == TwoWaySwap.genPairBeaconName(tokenId, tokenName, adaId, adaName))

        // ADA ("", "") sorts before any token (empty < non-empty hex), so asset1 = ADA
        assert(datum.asset1Id == adaId)
        assert(datum.asset1Name == adaName)
        assert(datum.asset1Beacon == TwoWaySwap.genAssetBeaconName(adaId, adaName))
        assert(datum.asset2Id == tokenId)
        assert(datum.asset2Name == tokenName)
        assert(datum.asset2Beacon == TwoWaySwap.genAssetBeaconName(tokenId, tokenName))

        // Prices should be swapped since assets were reordered
        assert(datum.asset1Price == price2) // ADA was second, now first
        assert(datum.asset2Price == price1) // token was first, now second

        assert(datum.prevInput == Option.None)
        assert(datum.expiration == Option.None)
    }

    test("genSwapDatum is order-independent") {
        val tokenId = testTokenSymbol
        val tokenName = ByteString.fromString("TestToken1")
        val adaId = ByteString.empty
        val adaName = ByteString.empty

        val datum1 = TwoWaySwap.genSwapDatum(
            tokenId, tokenName, adaId, adaName,
            Rational(1, 2), Rational(3, 1)
        )
        val datum2 = TwoWaySwap.genSwapDatum(
            adaId, adaName, tokenId, tokenName,
            Rational(3, 1), Rational(1, 2)
        )

        assert(datum1 == datum2)
    }
