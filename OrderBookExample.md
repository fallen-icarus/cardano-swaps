# Order Book Example

This document walks you through how to query the current order book for a given trading pair. To be
programming language agnostic, it will only involve using the command line. Koios' free
preproduction endpoints will be used.

## Target Order Book

I'm going to assume you know what trading pair you want. You will need to know each assets' on-chain
name (eg, policy ID and hexidecimal asset name). For this example, we will use ADA -> TestDJED (a
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
policy_id="47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd"
```

We just need to derive the `asset_names`. According to the One-Way Swap specification, the asset
name is: 

```txt
sha2_256( offer_id ++ offer_name ++ ask_id ++ ask_name )

But if ADA is part of the pair, swap it's policy id with "00".
```

So for the direction ADA -> TestDJED, ADA is the offer asset and TestDJED is the ask asset so the
derivation is:

```txt
sha2_256( "00" ++ "" ++ "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d" ++ "4f74686572546f6b656e0a" )
```

which simplifies to:

```txt
sha2_256( "00c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d4f74686572546f6b656e0a" )
```

Hashing this gives: `5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95`

So the One-Way swap beacon for ADA -> TestDJED is:

```bash
policy_id="47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd"
asset_name="5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95"
```

To determine the One-Way swap beacon name for the other direction (TestDJED -> ADA), you just need
to switch which asset is the offer and the ask in the `sha2_256` hash formula.

Here is the One-Way swap beacon for TestDJED -> ADA:

```bash
policy_id="47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd"
asset_name="5e09a478610895febe6afe42db300dd3bb985f3ff2e26125dbd4bf966d473350"
```

Now to query the One-Way swaps, we can use [this Koios
query](https://preprod.koios.rest/#post-/asset_utxos). We will need two separate queries, one for
each direction:

```bash
# ADA -> TestDJED
curl -X POST "https://preprod.koios.rest/api/v1/asset_utxos"  -H 'accept: application/json' -H 'content-type: application/json'  -d '{"_asset_list":[["47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd","5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95"]],"_extended":true}'

# TestDJED -> ADA
curl -X POST "https://preprod.koios.rest/api/v1/asset_utxos"  -H 'accept: application/json' -H 'content-type: application/json'  -d '{"_asset_list":[["47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd","5e09a478610895febe6afe42db300dd3bb985f3ff2e26125dbd4bf966d473350"]],"_extended":true}'
```

## Two-Way Swaps

Two-way swaps can go in either direction as long as they have the required asset for that direction.
But the process to query them is the same as with One-Way swaps.

Two-Way swap beacons all have the same policy id:
```bash
policy_id="84662c22dc5c0cadad7b2ebf9757ce9ea61dbd8fe64bc8c43c112a40"
```

Again, we just need to derive the `asset_names`. According to the Two-Way Swap specification, the asset
name is: 

```txt
sha2_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )

Sort the two assets in the trading pair lexicographically: the smaller asset is asset1 and the
larger one is asset2.

If ADA is part of the pair, swap it's policy id with "00" AFTER SORTING.
```

So unlike with One-Way swaps, the beacons for Two-Way swaps is independent of the swap direction.
Sorting ADA and TestDJED lexicographically results in ADA being `asset1` because the empty
bytestring comes first. Thus, the equation to use is:

```txt
sha2_256( "00" ++ "" ++ "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d" ++ "4f74686572546f6b656e0a" )
```

which simplifies to:

```txt
sha2_256( "00c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d4f74686572546f6b656e0a" )
```

Hashing this gives: `5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95`

Finaly, the Two-Way swap beacon for ADA <--> TestDJED is:

```bash
policy_id="84662c22dc5c0cadad7b2ebf9757ce9ea61dbd8fe64bc8c43c112a40"
asset_name="5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95"
```

Now we just need to query it using the same Koios query as before. Here is the exact command:

```bash
# ADA <--> TestDJED
curl -X POST "https://preprod.koios.rest/api/v1/asset_utxos"  -H 'accept: application/json' -H 'content-type: application/json'  -d '{"_asset_list":[["84662c22dc5c0cadad7b2ebf9757ce9ea61dbd8fe64bc8c43c112a40","5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95"]],"_extended":true}'
```

## Next Steps

At this point, you should have all of the current open orders for the trading pair. You just need to
filter out finished orders and organize them into a typical order book chart.

> [!NOTE]
> For filtering out empty orders, Koios actually allows filtering them out server-side. You just
> need to augment the above queries slightly. The following query will only return UTxOs that
> contain some of the native asset specified in the `cs.[]` part.
> ```bash
> curl -g -X POST -H "content-type: application/json" 'https://preprod.koios.rest/api/v1/asset_utxos?select=is_spent,asset_list&is_spent=eq.false&asset_list=cs.[{"policy_id":"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","asset_name":"4f74686572546f6b656e0a"}]' -d '{"_asset_list":[ ["84662c22dc5c0cadad7b2ebf9757ce9ea61dbd8fe64bc8c43c112a40","5e23340d7a9c22745a2f2f907c8c17a8962cfac2292a4cb1d3832b4b88cdee95"] ], "_extended": true }'
> ```
