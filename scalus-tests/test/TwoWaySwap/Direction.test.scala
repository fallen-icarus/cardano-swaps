package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class TwoWaySwapDirectionTest extends AnyFunSuite:

    val testTokenSymbol: ByteString =
        ByteString.fromHex("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d")

    test("getRequiredSwapDirection returns correct redeemer") {
        val tokenId = testTokenSymbol
        val tokenName = ByteString.fromString("TestToken1")
        val adaId = ByteString.empty
        val adaName = ByteString.empty

        // offering ADA (smaller), asking token → offer == min → TakeAsset1
        assert(
            TwoWaySwap.getRequiredSwapDirection(adaId, adaName, tokenId, tokenName) ==
                TwoWaySwapRedeemer.TakeAsset1
        )

        // offering token (larger), asking ADA → offer != min → TakeAsset2
        assert(
            TwoWaySwap.getRequiredSwapDirection(tokenId, tokenName, adaId, adaName) ==
                TwoWaySwapRedeemer.TakeAsset2
        )
    }
