# Order Book Example

This document walks you through how to query the current order book for a given trading pair. To be
programming language agnostic, it will only involve using the command line. Koios' free
preproduction endpoints will be used.

## Target Order Book

I'm going to assume you know what trading pair you want. You will need to know each assets' on-chain
name (eg, policy ID and hexadecimal asset name). For this example, we will use ADA -> TestDJED (a
test token I created using the always succeeding minting policy). These are their on-chain names:

```bash
# ADA
policy_id="" # The empty bytestring.
asset_name="" # The empty bytestring.

# TestDJED
policy_id="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
asset_name="4f74686572546f6b656e0a"
```

## One-Way Swaps

One-Way swap beacons all have the same policy id:
```bash
v2_policy_id="274765b4c626c28d18752176b59c0ff63db56b8305c1daa49c9879fe"
```

We just need to derive the `asset_names`. According to the One-Way Swap specification, the asset
name is: 

```txt
sha2_256( serialise_data( TradingPair(offer_id, offer_name, ask_id, ask_name) ) )
```

`serialise_data` is the CBOR encoding of the Plutus `Data` value (see the
[specification](README.md#beacon-naming-conventions)). In hex, `TradingPair` is `d87b9f` followed
by each field as a length-prefixed bytestring, then `ff`. An empty bytestring is `40`, a 28-byte
policy id is `581c` followed by the policy id, and an 11-byte asset name is `4b` followed by the
name.

So for the direction ADA -> TestDJED, ADA is the offer asset and TestDJED is the ask asset so the
derivation is:

```txt
sha2_256( "d87b9f" ++ "40" ++ "40" ++ "581c" ++ "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d" ++ "4b" ++ "4f74686572546f6b656e0a" ++ "ff" )
```

which simplifies to:

```txt
sha2_256( "d87b9f4040581cc0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d4b4f74686572546f6b656e0aff" )
```

Hashing this gives: `c676e361eda95d4c8e2651d123987203c22839df31d266f9d24d88e51f4817cb`

> [!IMPORTANT]
> The input to the hashing algorithm must be hexadecimally encoded. The above pre-hash is already in
> hexadecimal, but if you copy/paste it into the terminal it will likely be encoded as something
> else. You can test it with this [website](https://emn178.github.io/online-tools/sha256.html), but
> make sure to set the input encoding to 'Hex'.

> [!TIP]
> The `cardano-swaps` CLI can compute these names for you with `cardano-swaps beacon-info` (see
> [GettingStarted.md](GettingStarted.md)).

So the One-Way swap beacon for ADA -> TestDJED is:

```bash
v2_policy_id="274765b4c626c28d18752176b59c0ff63db56b8305c1daa49c9879fe"
asset_name="c676e361eda95d4c8e2651d123987203c22839df31d266f9d24d88e51f4817cb"
```

To determine the One-Way swap beacon name for the other direction (TestDJED -> ADA), you just need
to switch which asset is the offer and the ask in the `sha2_256` hash formula.

Here is the One-Way swap beacon for TestDJED -> ADA:

```bash
v2_policy_id="274765b4c626c28d18752176b59c0ff63db56b8305c1daa49c9879fe"
asset_name="a218175e55f900de49b4a4f38e03a36b13e369e947daa0117618c8acd42497c5"
```

Now to query the One-Way swaps, we can use [this Koios
query](https://preprod.koios.rest/#post-/asset_utxos). We will need two separate queries, one for
each direction:

```bash
# ADA -> TestDJED
curl -X POST "https://preprod.koios.rest/api/v1/asset_utxos"  -H 'accept: application/json' -H 'content-type: application/json'  -d '{"_asset_list":[["274765b4c626c28d18752176b59c0ff63db56b8305c1daa49c9879fe","c676e361eda95d4c8e2651d123987203c22839df31d266f9d24d88e51f4817cb"]],"_extended":true}'

# TestDJED -> ADA
curl -X POST "https://preprod.koios.rest/api/v1/asset_utxos"  -H 'accept: application/json' -H 'content-type: application/json'  -d '{"_asset_list":[["274765b4c626c28d18752176b59c0ff63db56b8305c1daa49c9879fe","a218175e55f900de49b4a4f38e03a36b13e369e947daa0117618c8acd42497c5"]],"_extended":true}'
```

## Two-Way Swaps

Two-way swaps can go in either direction as long as they have the required asset for that direction.
But the process to query them is the same as with One-Way swaps.

Two-Way swap beacons all have the same policy id:
```bash
v2_policy_id="ae19cf56a7631068aa754327e0472840b76e485868a324904c8b379e"
```

Again, we just need to derive the `asset_names`. According to the Two-Way Swap specification, the asset
name is: 

```txt
sha2_256( serialise_data( SortedPair(asset1_id, asset1_name, asset2_id, asset2_name) ) )

Sort the two assets in the trading pair lexicographically: the smaller asset is asset1 and the
larger one is asset2.
```

In hex, `SortedPair` is `d87a9f` followed by the fields, then `ff`.

So unlike with One-Way swaps, the beacons for Two-Way swaps is independent of the swap direction.
Sorting ADA and TestDJED lexicographically results in ADA being `asset1` because the empty
bytestring comes first. Thus, the equation to use is:

```txt
sha2_256( "d87a9f" ++ "40" ++ "40" ++ "581c" ++ "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d" ++ "4b" ++ "4f74686572546f6b656e0a" ++ "ff" )
```

which simplifies to:

```txt
sha2_256( "d87a9f4040581cc0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d4b4f74686572546f6b656e0aff" )
```

Hashing this gives: `9cfd4f3e253f2ed28b90c2498159d44cba323185ff3f98ac2fac7fdaa0b58f5e`

> [!IMPORTANT]
> The input to the hashing algorithm must be hexadecimally encoded. The above pre-hash is already in
> hexadecimal, but if you copy/paste it into the terminal it will likely be encoded as something
> else. You can test it with this [website](https://emn178.github.io/online-tools/sha256.html), but
> make sure to set the input encoding to 'Hex'.

Finaly, the Two-Way swap beacon for ADA <--> TestDJED is:

```bash
v2_policy_id="ae19cf56a7631068aa754327e0472840b76e485868a324904c8b379e"
asset_name="9cfd4f3e253f2ed28b90c2498159d44cba323185ff3f98ac2fac7fdaa0b58f5e"
```

Now we just need to query it using the same Koios query as before. Here is the exact command:

```bash
# ADA <--> TestDJED
curl -X POST "https://preprod.koios.rest/api/v1/asset_utxos"  -H 'accept: application/json' -H 'content-type: application/json'  -d '{"_asset_list":[["ae19cf56a7631068aa754327e0472840b76e485868a324904c8b379e","9cfd4f3e253f2ed28b90c2498159d44cba323185ff3f98ac2fac7fdaa0b58f5e"]],"_extended":true}'
```

## Next Steps

At this point, you should have all of the current open orders for the trading pair. You just need to
filter out finished orders and organize them into a typical order book chart.

> [!NOTE]
> For filtering out empty orders, Koios actually allows filtering them out server-side. You just
> need to augment the above queries slightly. The following query will only return UTxOs that
> contain some of the native asset specified in the `cs.[]` part.
> ```bash
> curl -g -X POST -H "content-type: application/json" 'https://preprod.koios.rest/api/v1/asset_utxos?select=is_spent,asset_list&is_spent=eq.false&asset_list=cs.[{"policy_id":"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","asset_name":"4f74686572546f6b656e0a"}]' -d '{"_asset_list":[ ["ae19cf56a7631068aa754327e0472840b76e485868a324904c8b379e","9cfd4f3e253f2ed28b90c2498159d44cba323185ff3f98ac2fac7fdaa0b58f5e"] ], "_extended": true }'
> ```
