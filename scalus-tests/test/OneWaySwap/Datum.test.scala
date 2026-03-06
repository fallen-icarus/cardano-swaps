package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class OneWaySwapDatumTest extends AnyFunSuite:

    test("genSwapDatum produces correct datum") {
        val offerId = ByteString.fromHex(
            "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
        )
        val offerName = ByteString.fromString("TestToken1")
        val askId = ByteString.empty
        val askName = ByteString.empty
        val price = Rational(1, 2)

        val datum = OneWaySwap.genSwapDatum(offerId, offerName, askId, askName, price)

        assert(datum.beaconId == OneWaySwap.beaconCurrencySymbol)
        assert(datum.pairBeacon == OneWaySwap.genPairBeaconName(offerId, offerName, askId, askName))
        assert(datum.offerId == offerId)
        assert(datum.offerName == offerName)
        assert(datum.offerBeacon == OneWaySwap.genOfferBeaconName(offerId, offerName))
        assert(datum.askId == askId)
        assert(datum.askName == askName)
        assert(datum.askBeacon == OneWaySwap.genAskBeaconName(askId, askName))
        assert(datum.swapPrice == price)
        assert(datum.prevInput == scalus.cardano.onchain.plutus.prelude.Option.None)
        assert(datum.expiration == scalus.cardano.onchain.plutus.prelude.Option.None)
    }
