# Cardano-Swaps

A [p2p-DeFi protocol](https://github.com/zhekson1/CSL-DeFi-Protocols) for swapping fungible tokens
on the Cardano Settlement Layer

> **Note** Knowledge of basic Haskell syntax and cardano-cli usage is recommended. For a list of
> everything that has changed from the previous version, see the [Changelog](CHANGELOG.md).

The Getting Started instructions can be found [here](./GettingStarted.md) and the benchmarks can
be found [here](./Benchmarks/).

---
## Table of Contents 
- [Abstract](#abstract)
- [Motivation](#motivation)
- [Preliminary Discussion](#preliminary-discussion)
- [Specification](#specification)
- [Features Discussion](#features-discussion)
- [Conclusion](#conclusion)

## Abstract

<!---
The core ideas of this protocol are:
1) One-way swaps: basic limit order.
2) Two-way swaps: naturally incentivizes providing liquidity.
3) All swaps are freely composable with other swaps which allow for arbitrary currency conversions.
4) Arbitragers are naturally incentivized to spread the liquidity across all trading pairs.
5) Concurrency scales with the number of users.
6) Users maintain full custody, delegation control, and voting control of their assets at all times.
--->

Cardano-Swaps is a p2p-DeFi protocol for swapping fungible tokens on the Cardano Settlement Layer
(CSL). It solves many of the pitfalls of current DEX implementations by empowering users to deploy
their own (and interact with each others') script addresses. This leads to the formation of an
order-book style "distributed" DEX, where liquidity arises from the aggregate of (composable) p2p
swaps. The protocol supports two kinds of swaps, one-way swaps and two-way swaps. One-way swaps
emulate the basic limit order. Meanwhile, two-way swaps provide a mechanism for users to profitably
provide liquidity within price ranges specified by the user. Since liquidity providers directly
profit from providing liquidity in the assets specified for that swap, the protocol naturally
incentives its own liquidity. Furthermore, since swaps can be freely composed, arbitragers are
naturally incentivized to spread the available liquidity across all trading pairs.

## Motivation

Many DEXes on Cardano are currently implemented in ways that lock users' assets into a tightly
fixed, and/or centrally maintained, set(s) of script addresses. Such design patterns are reminiscent
of the EVM's accounts-based programming paradigm, and inherit many of the same downsides;
scalability bottlenecks and asset/stake centralization. DEXes that hope to achieve massive scale on
the CSL must adhere to a radically different approach that takes full advantage of the composability
and availability guarantees offered by the eUTxO model.

## Preliminary Discussion

To appreciate the necessity of new DEX standards, it first important to understand the deficiencies
of the current status-quo:

### Current DEX Deficiencies  

One consequence of centralized script addresses is the necessity for liquidity pools and LP
providers as discrete, permissioned entities. LPs are a common feature of many DEXes and have a
number of downsides, namely slippage/impermanent-loss and lack of delegation control. Additionally,
current implementations of order-book style DEXes (which don't use LPs) suffer from the availability
challenges of permissioned batchers. No matter how performant a system of batchers is, their
resources do **not** scale in proportion to the number of users unless new batchers can join
permissionlessly when demand is high. Lastly, permissioned protocols lack composability with other
dApps; a critical feature for scaling eUTxO-based DeFi.

A non-exhaustive list of undesirable properties (and corresponding "workaround" solutions) is
outlined below. Even if some workarounds can be somewhat effective, the underlying design
*principles* are suboptimal.

| Undesirable Property | Workaround "Solution" | Issues |
| :--: | :--: | :--: |
| Impermanent Loss | Yield Farming & Concentrated Liquidity | Medium - Long term unsustainability |
| Incomplete or No Delegation Control | <ul><li>Asset pool fractionalization</li><li>Indirect Delegation via Governance Tokens</li></ul> | <ul><li>Unfair distribution of Governance tokens </li><li>Unavoidable centralization of stake (Major issue for Ouroboros)</li></ul>
| Scaling/Availability Bottlenecks | Batchers, Execution/Routing Engines, and/or other middlemen | Middlemen can take advantage of their position between users and the protocol. Even if MEV is mitigated, more users --> more execution demand --> possible centralization of middlemen if the batchers are not permissionless |


### Programmable Swaps

First proposed by Axo in their original [whitepaper](https://www.axo.trade/whitepaper.pdf),
programmable swaps are a more promising design choice than liquidity pools. Swaps are simply UTxOs
that may be consumed if and only if the resulting TX outputs satisfy the input script's logic. Swaps
can be fragmented across many user-controlled addresses, so delegation control is maintained. Users
then execute swaps from each other's addresses. Since each swap is atomic and explicitly defined, in
aggregate they are the optimal expression of (intra)market sentiment. This design pattern scales
naturally, since there must be at *least* as many swap addresses as there are users. 

### The Cardano-Swaps Protocol

Cardano-Swaps takes inspiration from Axo's programmable swaps design, adds delegation control as a
foundational feature, and, through the use of Beacon Tokens, removes the need for specialized
indexers. The only remaining bottleneck is the querying capacity of existing off-chain APIs, such as
Blockfrost or Koios. (This is not a limitation for users with powerful enough hardware, as they can
run their own API database).

## Specification

### High-Level Overview

The protocol is comprised of two types of composable swaps: a one-way swap and a two-way swap.
One-way swaps are effectively just limit orders (ie, they can only be executed at the specified
price or better). Most users will likely use the one-way swap.

Two-way swaps are a mechanism for profitably providing liquidity to the protocol without having to
resort to yield farming. These two-way swaps can go in either direction (eg, a two-way ADA <-> ERGO
swap can go ADA -> ERGO or ERGO -> ADA). By allowing the liquidity provider to specify separate
prices for each direction, they can profit from either direction of the swap. For example, if the
market rate for DJED <-> USDC is 1:1, Alice can set DJED -> USDC to 1.01 USDC per DJED and USDC ->
DJED to 1.01 DJED per USDC. This means Alice makes a 1% return no matter what direction is used.

Since the swaps are freely composable, arbitrarily complex swaps can be created. For example, Alice
can chain ADA -> ERGO with ERGO -> DUST to create a transaction that converts ADA -> DUST. This
opens up the possibility for arbitragers to find profitable arbitrage opportunities. This potential
for profit naturally incentivizes liquidity to spread across all trading pairs instead of it being
siloed into one trading pair as in TradFi and LP based DEXs.

Finally, through the use of beacon tokens, the protocol is fully p2p. No batchers are required
although they can be used if desired.

All of this is achieved will enabling users to maintain full spending custody, full delegation
control, and full voting control of their assets.

### Low-Level Overview

Each type of swap is comprised of a validator / beacon policy pair. This means that each type of
swap gets its own universal address. For example, all of Alice's one-way swaps, regardless of the
trading pair, will be located at address X and all all of Alice's two-way swaps, regardless of the
trading pair, will be located at address Y. Furthermore, since each type of swap has its own beacon
policy, each type is easily distinguishable off-chain while still being easily queryable by end
users.

:exclamation: While the spec shows one-way swap datums and redeemers having the same names as the
two-way swap datums and redeemers *they are distinct data types*. Imagine they are in separate name
spaces.

#### One-Sway Swaps

One-way swaps use one universal validator script for all swap pairs, and one minting
script for each offer asset. For example, all trading pairs where ADA is being offered (eg,
DJED/ADA, AGIX/ADA) will share a minting policy while all trading pairs where DJED is being offered
will get a different minting policy. The asset name for all beacons is:

```Bash
sha2_256( ask_policy_id ++ ask_asset_name )
```

Therefore, the beacon policy id represents the offer asset and the beacon asset name represents the
ask asset. This allows for more expressive queries since it makes it possible to query all swaps
where XYZ is being offered, regardless of the asset being asked for, in addition to being able to
query all swaps for a specific trading pair. This extra expressiveness improves the arbitrager's
ability to find profitable paths through currently open swaps.

##### Swap Address

All users get a single swap address where all of their one-way swaps will be located. This universal
validator can enforce all possible swaps for the user. And since users only need to worry about a
single address, it dramatically simplifies the user experience.

##### AssetConfig

In order to create a unique beacon minting policy for each offer asset, the `AssetConfig` extra
parameter is used in addition to the validator hash of the spending policy. Here is the definition
for the `AssetConfig`:

```Haskell
type AssetConfig = (CurrencySymbol,TokenName)
```

**Every possible `AssetConfig` will have its own beacon minting policy.**

##### Minting Redeemers

Two minting redeemers are introduced here, their usage is explained further below.

```Haskell
data BeaconRedeemer
  = CreateSwap [AssetConfig] -- ^ A list of assets being asked for.
  | BurnBeacons
```

##### Validator Redeemers

Two validator redeemers are introduced here, their usage is explained further below.

```Haskell
data SwapRedeemer
  = CloseOrUpdate
  | Swap
```

Only the owner (signified by the address' staking credential) can use the `CloseOrUpdate` redeemer.
Anyone can use the `Swap` redeemer as long as the swap conditions are met.

##### Datums
Inline datums are used for all UTxOs. All UTxOs get the same type of datum:

``` Haskell
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol -- ^ Beacon policy id for this swap.
  , beaconName :: TokenName -- ^ Beacon asset name for this swap.
  , offerId :: CurrencySymbol -- ^ Offer policy id for this swap.
  , offerName :: TokenName -- ^ Offer asset name for this swap.
  , askId :: CurrencySymbol -- ^ Ask policy id for this swap.
  , askName :: TokenName -- ^ Ask asset name for this swap.
  , swapPrice :: Rational -- ^ The desired swap ratio.
  , prevInput :: Maybe TxOutRef -- ^ The output reference for the corresponding swap input.
  }
```

The beacon information in the datum prevents misuse of beacons. The validator forces all beacons to
be burned instead of being withdrawn. This ensures that beacons can only ever be located at
swap addresses. **If the wrong beacon information is supplied, assets can be locked forever.** For
this reason, all minting policies check that the datums have the proper information; the mint
transaction will fail if they do not.

The `Rational` type is a fraction (decimal types do not work on-chain). All prices in Cardano-Swaps
are relative (similar to limit orders in an order-book exchange). Swaps are always priced in
askedAsset/offeredAsset. For example, if ADA is being offered for DUST at a price of 1.5
(converted to 3/2), the contract requires that 3 DUST are deposited for every 2 ADA removed from
the swap address. Ratios of DUST:ADA >= 3/2 will pass, while ratios < 3/2 will pass. 

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets at
in the UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for
DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same Tx. Since every
user explicitly defines their desired swap ratios, oracles are not required. The "global" price
naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of Lovelace.**

##### Creating a One-Way Swap

Swap "writers" must first create a swap by using the `CreateSwap` Redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1. Only the beacons for the target ask assets can be minted/burned - even if they are being burned,
   they must appear in the redeemer.
2. The token names for the corresponding beacons must be: 
   `sha2_256( ask_policy_id ++ ask_asset_name )`
3. The beacons are minted to an address whose payment credential is of the universal spending
   validator for one-way swaps.
4. The beacons are minted to an address with a staking credential (either a pubkey or a script).
5. The beacons must be stored individually (i.e., 1 per UTxO) at the address.
6. The datum of the output containing the beacon must have the proper inline datum:
      - `beaconId` == this policy id
      - `beaconName` == `sha2_256( ask_policy_id ++ ask_asset_name )`
      - `offerId` == `CurrencySymbol` of the extra parameter passed to the minting policy.
      - `offerName` == `TokenName` of the extra parameter passed to the minting policy.
      - `askId` == `CurrencySymbol` for the corresponding `AssetConfig` in the redeemer.
      - `askName` == `TokenName` for the corresponding `AssetConfig` in the redeemer.
      - `price` denominator > 0.
      - `price` > 0.
7. The beacons must be stored with some of the offer asset.
8. No extraneous assets can be stored in the swap UTxO.

Once the beacon is minted to the swap address, the spending script does not allow closing the swap
*unless* the beacon is being burned. This prevents beacons from being sent to unrelated addresses.

All swaps must be stored with the proper beacon. Due to this requirement, the absolute minimum
possible value for a swap UTxO to have is 1.823130 ADA - this is if the offer asset is ADA. This
minimum value can be reclaimed upon closing the swap so it should be thought of as a deposit. **All
open swaps require a deposit of at least 1.823130 ADA.** While this behavior is due to the current
protocol parameters, it is actually a good thing since it helps prevents denial-of-service attacks
from having a lot of "zombie swaps" open (i.e., UTxOs where there is not enough of the offer asset
for other users to swap).

It is possible to open swaps for multiple different trading pairs in a single transaction. The
minting policy is capable of still ensuring that all beacons are stored properly (with the proper
value and datum). For example, since each beacon's datum is specific to that beacon, mixing up
beacons and datums will cause the minting transaction to fail.

This redeemer allows burning any beacons to enable composition with the `CloseOrUpdate` redeemer.
More on this later.


##### Closing or Updating a Swap

Open Swap UTxOs can be closed or updated by the address owner (signified by the address' staking
credential).

The `CloseOrUpdate` redeemer allows the owner to recover the deposit stored with the swap. **In
order to reclaim the deposit, the beacon(s) must be burned.** As the redeemer name suggests, swaps
can also be updated inplace instead. The requirements for successfully using the `CloseOrUpdate`
redeemer are:

1. The staking credential must signal approval (via key signing or staking script execution).
2. Any beacons not burned must be re-output to a dApp address with staking and with the proper 
   inline SwapDatum:
    - all fields the same as the input datum except for the updated price.
    - `price` denominator must be > 0.
    - `price` must be > 0.
3. The beacons must be stored with some of the offer asset.
4. No extraneos assets can be stored in the swap UTxO.
5. The beacons must be stored individually.

Requirement 2 guarantees that beacons from other trading pairs cannot be combined into one output
UTxO; all trading pairs must get their own swap UTxOs.

Requirement 5 forces the user to burn extra beacons when swaps are being consolidated. 

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need
to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must
be minted, the `CreateSwap` redeemer must be used. This behavior enables the swap owner to change
what trading pair a swap is for in a single transaction. This is instead of having to close the
original swap in one transaction only to re-open it in another transaction. When this composition is
used, the `CreateSwap` redeemer must contain both the original ask asset and the new ask asset in
the `AssetConfig` list.

##### Executing a Swap

Any Cardano user can execute an available swap using the `Swap` Redeemer as long as the swap
conditions are met.

At a high level, the `Swap` redeemer uses the swap input's output reference and datum to find the
corresponding swap output. The checks are essentially:
1) Does this output have the beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?
4) If "Yes", this is the corresponding output.

It then compares the value of that output to the input to determine the swap's asset flux. Since the
validator first checks for the beacon, each execution is dedicated for a specific trading pair. Any
other outputs are ignored in this specific execution. This logic works because a script is executed
once for every UTxO spent from the address. If input 1 is for beacon XYZ and input 2 is for beacon
ABC, the first execution can be dedicated to beacon XYZ and the second execution can be dedicated to
ABC. The net transaction will only succeed if all executions succeed. This behavior allows cheaply
composing swaps of different trading pairs that are located at the same address.

At a low-level, for a swap execution to be successfull, all of the following must be true:

1. The input must contain the beacon for that trading pair - the required beacon is gotten from the
   datum for that UTxO. If the beacon is present, that means the datum can be trusted.
2. There must be an output to this address with the proper value and inline `SwapDatum`:
    - must contain exactly 1 of the proper beacon.
    - all fields the same as the input datum except the `prevInput` field must be `Just(input_ref)`
      where `input_ref` is the corresponding input's output reference.
3. Offered asset taken * price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. ADA can always be
deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner
and that swap inputs have a valid `price`: denominator > 0 and `price` > 0.

Requirements 2 & 4 guarantee that beacons from other trading pairs cannot be combined into one output
UTxO. This as two beneficial consequences:
1) The swap logic is very simple.
2) All swap UTxOs are as small as possible which maximizes how many swaps can be composed in a
single Tx as well as how easily the beacons can be queried.

Custom error messages are included to help troubleshoot why a swap failed.


#### Two-Sway Swaps

Two-way swaps use one universal validator script for all swap pairs, and one universal minting
script for all trading pairs (this is different than with one-way swaps). The asset name for
all beacons is:

```Bash
sha_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )
```

Where asset1 and asset2 are the **sorted** trading pair. Trading pairs are sorted by name. For
example, for the trading pair ADA <-> DJED, the empty bytestring (on-chain representation for ADA's
`CurrencySymbol`) is less than DJED's `CurrencySymbol` which means, for this trading pair, asset1 is
ADA and asset2 is DJED. By sorting the names first, trading pairs will always have one beacon
regardless of swap direction (eg, ADA -> DJED or DJED -> ADA).

Therefore, the beacon policy id represents *two-way swap* and the beacon asset name represents the
trading pair. 

##### Swap Address

All users get a single swap address where all of their two-way swaps will be located. This universal
validator can enforce all possible swaps for the user. And since users only need to worry about a
single address, it dramatically simplifies the user experience.

##### AssetConfig

The `AssetConfig` variable is used with a redeemer for the minting policy to tell the policy what
trading pair a swap is being created for. Here is the definition for the `AssetConfig` (it is the
same as with the one-way swap):

```Haskell
type AssetConfig = (CurrencySymbol,TokenName)
```

##### Minting Redeemers

Two minting redeemers are introduced here, their usage is explained further below.

```Haskell
data BeaconRedeemer
  = CreateSwap [(AssetConfig,AssetConfig)] -- ^ A list of trading pairs to create swaps for.
  | BurnBeacons
```

##### Validator Redeemers

Three validator redeemers are introduced here, their usage is explained further below.

```Haskell
data SwapRedeemer
  = CloseOrUpdate
  | ForwardSwap
  | ReverseSwap
```

Only the owner (signified by the address' staking credential) can use the `CloseOrUpdate` redeemer.
Anyone can use the `ForwardSwap` or `ReverseSwap` redeemers as long as the swap conditions are met.

##### Datums
Inline datums are used for all UTxOs. All UTxOs get the same type of datum:

``` Haskell
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol -- ^ The beacon policy id for two-way swaps.
  , beaconName :: TokenName -- ^ The asset name for the beacon for this trading pair.
  , asset1Id :: CurrencySymbol -- ^ The policy id for the first asset in the sorted pair.
  , asset1Name :: TokenName -- ^ The asset name for the first asset in the sorted pair.
  , asset2Id :: CurrencySymbol -- ^ The policy id for the second asset in the sorted pair.
  , asset2Name :: TokenName -- ^ The asset name for the second asset in the sorted pair.
  , forwardPrice :: Rational -- ^ The swap price as a fraction: Asset1/Asset2.
  , reversePrice :: Rational -- ^ The swap price as a fraction: Asset2/Asset1.
  , prevInput :: Maybe TxOutRef -- ^ The output reference for the corresponding swap input.
  }
```

The beacon information in the datum prevents misuse of beacons. The validator forces all beacons to
be burned instead of being withdrawn. This ensures that beacons can only ever be located at
swap addresses. **If the wrong beacon information is supplied, assets can be locked forever.** For
this reason, all minting policies check that the datums have the proper information; the mint
transaction will fail if they do not.

The `Rational` type is a fraction (decimal types do not work on-chain). All prices in Cardano-Swaps
are relative (similar to limit orders in an order-book exchange). Swaps are always priced in
askedAsset/offeredAsset. For example, if ADA is being offered for DUST at a price of
1.5 (converted to 3/2), the contract requires that 3 DUST are deposited for every 2 ADA removed from
the swap address. Ratios of DUST:ADA >= 3/2 will pass, while ratios < 3/2 will pass.

Since all prices are askedAsset/offeredAsset, asset2 is being offered in `forwardPrice` and asset 1
is being offered in `reversePrice`.

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets at
in the UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for
DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same Tx. Since every
user explicitly defines their desired swap ratios, oracles are not required. The "global" price
naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of Lovelace.**

##### Creating a Two-Way Swap

Swap "writers" must first create a swap by using the `CreateSwap` Redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1. The first asset in the sorted trading pair is asset1 and the second asset is asset2.
2. Only the beacons for the target trading pairs can be minted/burned - even if they are being burned,
   they must appear in the redeemer.
3. The token names for the corresponding beacons must be: 
    `sha_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )`
4. The beacons are minted to an address whose payment credential is of the universal spending
   validator for one-way swaps.
5. The beacons are minted to an address with a staking credential (either a pubkey or a script).
6. The beacons must be stored individually (i.e., 1 per UTxO) at the address.
7. The datum of the output containing the beacon must have the proper inline datum:
      - `beaconId` == this policy id
      - `beaconName` == `sha2_256( ask_policy_id ++ ask_asset_name )`
      - `asset1Id` == `CurrencySymbol` of asset1.
      - `asset1Name` == `TokenName` of asset1.
      - `asset2Id` == `CurrencySymbol` of asset2.
      - `asset2Name` == `TokenName` of asset2.
      - `forwardPrice` denominator > 0.
      - `forwardPrice` > 0.
      - `reversePrice` denominator > 0.
      - `reversePrice` > 0.
7. The beacons must be stored with asset1 and/or asset2.
8. No extraneous assets can be stored in the swap UTxO.

Once the beacon is minted to the swap address, the spending script does not allow closing the swap
*unless* the beacon is being burned. This prevents beacons from being sent to unrelated addresses.

All swaps must be stored with the proper beacon. Due to this requirement, the absolute minimum
possible value for a swap UTxO to have is about 1.8 ADA - this is if the offer asset is ADA. This
minimum value can be reclaimed upon closing the swap so it should be thought of as a deposit. **All
open swaps require a deposit of at least 1.8 ADA.** While this behavior is due to the current
protocol parameters, it is actually a good thing since it helps prevents denial-of-service attacks
from having a lot of "zombie swaps" open (i.e., UTxOs where there is not enough of the assets for
other users to swap).

It is possible to open swaps for multiple different trading pairs in a single transaction. The
minting policy is capable of still ensuring that all beacons are stored properly (with the proper
value and datum). For example, since each beacon's datum is specific to that beacon, mixing up
beacons and datums will cause the minting transaction to fail.

This redeemer allows burning any beacons to enable composition with the `CloseOrUpdate` redeemer.
More on this later.


##### Closing or Updating a Swap

Open Swap UTxOs can be closed or updated by the address owner (signified by the address' staking
credential).

The `CloseOrUpdate` redeemer allows the owner to recover the deposit stored with the swap. **In
order to reclaim the deposit, the beacon(s) must be burned.** As the redeemer name suggests, swaps
can also be updated inplace instead. The requirements for successfully using the `CloseOrUpdate`
redeemer are:

1. The staking credential must signal approval (via key signing or staking script execution).
2. Any beacons not burned must be re-output to as dApp address with staking and with the proper 
   inline SwapDatum:
    - all fields the same as the input datum except for the updated price.
    - `forwardPrice` denominator > 0.
    - `forwardPrice` > 0.
    - `reversePrice` denominator > 0.
    - `reversePrice` > 0.
3. The beacons must be stored with asset1 and/or asset2.
4. No extraneos assets can be stored in the swap UTxO.
5. The beacons must be stored individually.

Requirement 2 guarantees that beacons from other trading pairs cannot be combined into one output
UTxO; all trading pairs must get their own swap UTxOs.

Requirement 5 forces the user to burn extra beacons when swaps are being consolidated. 

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need
to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must
be minted, the `CreateSwap` redeemer must be used. This behavior enables the swap owner to change
what trading pair a swap is for in a single transaction. This is instead of having to close the
original swap in one transaction only to re-open it in another transaction. When this composition is
used, the `CreateSwap` redeemer must contain both the original ask asset and the new ask asset in
the `AssetConfig` list.

##### Executing a Swap

Any Cardano user can execute an available swap using either the `ForwardSwap` or `ReverseSwap`
redeemer as long as the swap conditions are met. When `ForwardSwap` is used, the `forwardPrice` is
used and asset2 is the offer asset while asset1 is the ask asset. When `ReverseSwap` is used, the
`reversePrice` is used and asset1 is the offer asset and asset2 is the ask asset.

At a high level, the validator uses the swap input's output reference and datum to find the
corresponding swap output. The checks are essentially:
1) Does this output have the beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?
4) If "Yes", this is the corresponding output.

It then compares the value of that output to the input to determine the swap's asset flux. Since the
validator first checks for the beacon, each execution is dedicated for a specific trading pair. Any
other outputs are ignored in this specific execution. This logic works because a script is executed
once for every UTxO spent from the address. If input 1 is for beacon XYZ and input 2 is for beacon
ABC, the first execution can be dedicated to beacon XYZ and the second execution can be dedicated to
ABC. The net transaction will only succeed if all executions succeed. This behavior allows cheaply
composing swaps of different trading pairs that are located at the same address.

At a low-level, for a swap execution to be successfull, all of the following must be true:

1. The input must contain the beacon for that trading pair - the required beacon is gotten from the
   datum for that UTxO. If the beacon is present, that means the datum can be trusted.
2. There must be an output to this address with the proper value and inline `SwapDatum`:
    - must contain exactly 1 of the proper beacon.
    - all fields the same as the input datum except the `prevInput` field must be `Just(input_ref)`
      where `input_ref` is the corresponding input's output reference.
3. Offered asset taken * price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. ADA can always be
deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner
and that swap inputs have a valid price: denominator > 0 and price > 0.

Requirements 2 & 4 guarantee that beacons from other trading pairs cannot be combined into one output
UTxO. This as two beneficial consequences:
1) The swap logic is very simple.
2) All swap UTxOs are as small as possible which maximizes how many swaps can be composed in a
single Tx as well as how easily the beacons can be queried.

Custom error messages are included to help troubleshoot why a swap failed.


## Features Discussion

A discussion of features unique to Cardano-Swaps, in addition to the features shared by all p2p-DeFi
protocols.

### Composable Swaps

Since multiple swaps are combinable into a single transaction, any arbitrarily complex swap
transaction can be created. The only limits are the size and execution budgets of transactions,
which are Cardano protocol parameters.

### Emergent Liquidity

Liquidity in cardano-swaps is an *emergent* property; it arises from the (healthy) incentives for
users to provide liquidity with two-way swaps and arbitragers to compose complex swaps. As long as
the entry and exit swap-pairs have enough liquidity, arbitragers can spread liquidity into less
liquid swap pairs. As a bonus, **the very nature of *illiquidity* implies great arbitrage
opportunities**. The more illiquid a swap pair, the greater the potential arbitrage profits.
Providing liquidity and participating in arbitrage/market-making is permissionless, so anyone can
create their own strategies/algorithms to provide liquidity or find the most profitable "path"
through the sea of available swaps.

### Offer Based Queries

Offer based queries allow arbitragers to find profitable swap compositions based on current market
demand. The previous version required the arbitrager to know what trading pairs to check in advance.
But what if there was a token that they had never heard of? This opportunity would be missed. With
offer based queries, these unheard of tokens would be returned by the first query and the
arbitrager's algorithm can decide which other assets to query based on those results.

In short, the previous version effectively required hard-coded composition paths while the new
version allows for natural discovery of composition paths.

### Single Swap Addresses

This dramatically improves the usability of the DEX since users or frontends don't need to manage a
possibly infinite set of addresses for users.

### Benchmarks and Fee Estimations (YMMV)

The protocol is capable of handling up to 15 swaps in a single transaction, regardless of the
composition of one-way and two-way swaps in the transaction.

**No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found in the [Benchmarks](./Benchmarks/) folder.

### Swap Collisions

Recall the composition example above. What would happen if another user simultaneously submits a
transaction using *one* of the same "Swap" UTxOs as an input? Whichever transaction is processed by
(most) nodes first will succeed, and the other will fail entirely. Arbitragers/market-makers will be
in constant competition with one another to query and execute the most profitable swaps at any point
in time, and must strike a balance between simple swaps and complex swaps. There is a lot of
stochasticity here; the higher the ratio of open swaps to arbitragers, the lower the overall
chances of a collision, but the more attractive a particular swap is, the higher the chances of
*that* particular collision. And "attractiveness" depends not only on the price of one swap, but how
that price relates to all possible compositions involving that swap. The fact there there can
potentially be an infinite number of paths through all open swaps, arbitragers can profit without
having to rely on the same paths.


## Conclusion

The Cardano-Swaps protocol has all of the desired properties of a highly composable p2p-DEX. Thanks
to the use of Beacon Tokens, decentralization is no longer limited by the design of DEXs. Instead,
the limiting factor is now the off-chain querying. However, innovations in this space are still in
the early days. The Koios API is an example of a more decentralized off-chain platform. As the
technology improves, so too will the decentralization of the protocol.
