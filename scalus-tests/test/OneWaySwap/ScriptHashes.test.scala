package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class OneWaySwapScriptHashTest extends AnyFunSuite:

    test("swap script hash matches plutus.json") {
        val expected = ByteString.fromHex(
            "1d6cff26bcab91d2061aad0bd259cbb7d76d25ced2eeaed5926a42ad"
        )
        assert(OneWaySwap.swapScriptHash == expected)
    }

    test("beacon script hash (unapplied) matches plutus.json") {
        val expected = ByteString.fromHex(
            "7fe8bf3c2f93f9929bad79104e468331ed251c152844cfd44431e3b9"
        )
        assert(OneWaySwap.rawBeaconScriptHash == expected)
    }
