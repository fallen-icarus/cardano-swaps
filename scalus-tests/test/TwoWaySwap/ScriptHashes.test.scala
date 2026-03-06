package cardanoswaps

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class TwoWaySwapScriptHashTest extends AnyFunSuite:

    test("swap script hash matches plutus.json") {
        val expected = ByteString.fromHex(
            "11928a3ac3b65edbf103ea6bb3362e39b879a36f02897df31c40917b"
        )
        assert(TwoWaySwap.swapScriptHash == expected)
    }

    test("beacon script hash (unapplied) matches plutus.json") {
        val expected = ByteString.fromHex(
            "ca7f3516f4467e2160f45a87c8d07a6a5c6544763d12b3f636aa9577"
        )
        assert(TwoWaySwap.rawBeaconScriptHash == expected)
    }
