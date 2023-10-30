# Cardano-Swaps

A [p2p-DeFi protocol](https://github.com/zhekson1/CSL-DeFi-Protocols) for trustlessly swapping
fungible tokens on the Cardano Settlement Layer. All users maintain full custody, delegation
control, and voting control of their assets at all times. No batchers are required.

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
- [FAQ](#faq)
- [Conclusion](#conclusion)

## Abstract

Cardano-Swaps is a p2p-DeFi protocol for swapping fungible tokens on the Cardano Settlement Layer
(CSL). It solves many of the pitfalls of current DEX implementations by empowering users to deploy
their own (and interact with each others') script addresses. This leads to the formation of an
order-book style "distributed" DEX. The protocol supports two kinds of swaps, one-way swaps and
two-way swaps. One-way swaps emulate the basic limit order. Meanwhile, two-way swaps provide a
mechanism for users to profitably provide liquidity at prices specified by the user. Since liquidity
providers directly profit from providing liquidity in the assets specified for that swap, the
protocol naturally incentives its own liquidity without having to resort to the questionable
economics of yield farming. Furthermore, since swaps can be freely composed, arbitragers are
naturally incentivized to spread the available liquidity across all trading pairs. This protocol is
natively composable with all other p2p protocols.

## Motivation

Many DEXes on Cardano are currently implemented in ways that lock users' assets into a tightly
fixed, and/or centrally maintained, set(s) of script addresses. Such design patterns are reminiscent
of the EVM's accounts-based programming paradigm, and inherit many of the same downsides;
scalability bottlenecks, expressiveness restrictions, and asset/stake centralization. DEXes that
hope to become the foundation for a healthy and fully trustless, decentralized economy must adhere
to a radically different approach that takes full advantage of the composability and availability
guarantees offered by the eUTxO model.

## Preliminary Discussion

To appreciate the necessity of new DEX standards, it is first important to understand the
deficiencies of the current status-quo:

### Current DEX Deficiencies  

One consequence of centralized script addresses is the necessity for liquidity pools and LP
providers as discrete, (usually) permissioned entities. LPs are a common feature of many DEXes and
have a number of downsides (discussed shortly). Additionally, current implementations of order-book
style DEXes (which don't use LPs) suffer from the availability challenges of permissioned batchers.

##### Why LPs are not the right architecture for DEXs (in no particular order)

- No matter how performant a system of batchers is, their resources do **not** scale in proportion
to the number of users unless new batchers can join permissionlessly when demand is high. 
- Since an economy's health is proportional to how easily and accurately price discovery can occur,
DEXs that do not allow users to freely express their own desired prices can only result in an
unhealthy blockchain economy. Furthermore, impermanent loss makes it impossible for users to properly
manage risk.
- Permissioned protocols lack trustless composability with other dApps; a critical feature for both
scaling eUTxO-based DeFi and building a fully featured, trustless blockchain economy.
- The concentration of assets directly undermines the security assumptions of PoS blockchains. This
ultimately means it is **impossible** for LPs to serve as the foundation of a trustless and
decentralized economy.

##### Why the workarounds for the issues of LPs are not sufficient

- Impermanent Loss - yield farming is ultimately printing a useless token to make Alice whole for
being forced to sell at a price she otherwise would not have sold. To put it bluntly, Alice lost
$100 of a stablecoin and was given some economically useless token as compensation. Add since the
yield tokens are being minted, the value of the already useless token can only decrease over time.
Without the yield tokens, there is literally no incentive to provide liquidity to LPs so if yield
farming ever stopped, the DEX would collapse. Yield farming is a currency crisis waiting to happen.
No professional institutions can comply with regulations and manage risk in this context. In short,
mass adoption of LP based DEXs by professional institutions is impossible.
- No way for users to fully express their own desired prices - LPs base the exchange rates on supply
and demand of the assets in the LP. But this is wholey insufficient to properly reflect the true
market sentiment. To quote [investopedia](https://www.investopedia.com/terms/p/pricediscovery.asp),
"The process of price discovery looks at a number of tangible and intangible factors, including
supply and demand, investor risk attitudes, and the overall economic and geopolitical environment.
Simply put, it is where a buyer and a seller agree on a price and a transaction occurs." There is
fundamentally no way for algorithms that are based soley on supply and demand to properly reflect
the true market sentiment. And when market sentiment cannot be accurately reflected, misallocation
of resources is inevitable (ie, an unhealthy economy).
- Batchers can always take advantage of their priviledged position as middle-men - MEV will always
be an issue as long as using middle-men is required. Permissioned batchers exacerbate this issue
since only the "chosen few" can even have this opportunity.
- Batcher based protocols cannot trustlessly compose - since LPs require going through middle-men,
the ultimate transaction for Alice will likely not even be seen by Alice prior to submission to the
blockchain. How can Alice express that she wants her swap composed with an options contract and
expect it to be trustlessly executed? This is not an issue of standards. The only way for the
composition to be trustless is if the smart contract itself enforced it. This requires two things:
the smart contract must support **arbitrary** compositions and the logic must be extremely cheap
since the logic of the actual dApps must also be executed in the same transaction. These
requirements are fundamentally impossible to satisfy. Smart contracts are effectively embedded
systems that only have so much memory and steps available in a given transaction. The current limits
are not even close to the required limits to satisfy the above two requirements. **P2P protocols do
not have the same limitations since Alice personally creates and signs her own transaction.** The
above two requirements are only requirements when middle-men are creating and executing transactions
on the behalf of users.
- No delegation control or voting control while using the DEX - DEXs try to issue governance tokens
to get around this but fair distribution of the governance tokens is rarely accomplished (it is a
very hard problem). Even if the governance tokens were distributed fairly, they are largely just for
show since the DEX creators can still choose to go against the vote. There is no trustless
connection between the governance tokens and what actually happens with the stake/dApp. Finally,
even if there was a trustless connection, there is no way to actually have all users express their
own stake preferences when the stake is decided democratically. It is almost inevitable that the
voting outcome will go against the wishes of some users. No matter how you look at it, LP based
dApps are an existenstial problem for PoS blockchains. LPs can never be used as the foundation for a
trustless and decentralized blockchain economy.


### Programmable Swaps

First proposed by Axo in their original [whitepaper](https://www.axo.trade/whitepaper.pdf),
programmable swaps are a more promising design choice than liquidity pools. Swaps are simply UTxOs
that may be consumed if and only if the resulting TX outputs satisfy the input script's logic. Swaps
can be fragmented across many user-controlled addresses, so delegation control and voting control is
maintained. Users then execute swaps from each other's addresses. Since each swap is atomic and
explicitly defined, in aggregate they are the optimal expression of (intra)market sentiment. This
design pattern scales naturally, since there must be at *least* as many swaps as there are users. 

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
for profit naturally incentivizes liquidity to be spread across all trading pairs instead of it
being siloed into one trading pair as in TradFi and LP based DEXs.

Finally, through the use of beacon tokens, the protocol is fully p2p. No batchers are required
although they can be used if desired.

All of this is achieved while still enabling users to maintain full spending custody, full
delegation control, and full voting control of their assets.

### Low-Level Overview

Each type of swap is comprised of a validator / beacon policy pair. This means that each type of
swap gets its own universal address. For example, all of Alice's one-way swaps, regardless of the
trading pair, will be located at address X and all all of Alice's two-way swaps, regardless of the
trading pair, will be located at address Y. Address X is != address Y. Furthermore, since each type
of swap has its own beacon policy, each type is easily distinguishable off-chain while still being
easily queryable by end users.

:exclamation: While the spec shows one-way swap datums and redeemers having the same names as the
two-way swap datums and redeemers *they are distinct data types*. Imagine they are in separate name
spaces.

#### One-Sway Swaps

One-way swaps use one universal validator script for all swap pairs, and one universal minting
policy for all one-way beacons. There are two beacons for one-way swaps: the trading pair beacon and
the offer beacon.

The offer beacon asset name is `sha2_256( offer_policy_id ++ offer_asset_name )`.

The trading pair beacon asset name is 
`sha2_256( offer_id ++ offer_name ++ ask_id ++ ask_name )`. However, in the case where one
of the assets is ADA (which is just the empty bytestring), the ADA policy id is set to "00" instead.
Without this changed, there would be no difference between the offer beacon and trading pair beacon
when ADA is part of a swap.

These beacons allow for expressive queries since it makes it possible to query all swaps where XYZ
is being offered, regardless of the asset being asked for. This is in addition to being able to
query all swaps for a specific trading pair. This extra expressiveness improves the arbitrager's
ability to find profitable paths through currently open swaps.

##### Swap Address

All users get a single swap address where all of their one-way swaps will be located. This universal
validator can enforce all possible swaps for the user. And since users only need to worry about a
single address, it keeps the UI simple.

##### Minting Redeemers

Two minting redeemers are introduced here, their usage is explained further below.

```Haskell
data BeaconRedeemer
  = CreateSwap 
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
  , pairBeacon :: TokenName -- ^ Trading pair beacon asset name for this swap.
  , offerId :: CurrencySymbol -- ^ Offer policy id for this swap.
  , offerName :: TokenName -- ^ Offer asset name for this swap.
  , offerBeacon :: TokenName -- ^ Offer beacon asset name for this swap.
  , askId :: CurrencySymbol -- ^ Ask policy id for this swap.
  , askName :: TokenName -- ^ Ask asset name for this swap.
  , swapPrice :: Rational -- ^ The desired swap ratio: Ask/Offer.
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

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets
in the UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for
DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same Tx. Since every
user explicitly defines their desired swap ratios, oracles are not required. The "global" price
naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of Lovelace.**

##### Creating a One-Way Swap

Swap "writers" must first create a swap by using the `CreateSwap` Redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1) The beacons must go to an addres protected by the dApp validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    a) Exactly two kinds of beacons: pair beacon and offer beacon.
    b) The beacons must correspond to the beacons in the datum.
    c) There must be exactly 1 of each beacon.
    d) No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    a) `beaconId` == this policy id.
    b) `pairBeacon` == sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name)
    c) `offerId` == asset_id of the offer asset.
    d) `offerName` == asset_name of the offer asset.
    e) `offerBeacon` == sha2_256(offer_id ++ offer_name).
    f) `askId` == asset_id of the ask asset.
    g) `askName` == asset_name of the ask asset.
    k) `swapPrice` denominator > 0
    l) `swapPrice` > 0
5) The offer asset and ask assets must be different assets.

Once the beacons are minted to the swap address, the spending script does not allow closing the swap
*unless* the beacons are being burned. This prevents beacons from being sent to unrelated addresses
and guarantees the integrity of the off-chain queries.

All swaps must be stored with the proper beacons. Due to this requirement, the absolute minimum
possible value for a swap UTxO to have is about 2 ADA. This minimum value can be reclaimed upon
closing the swap so it should be thought of as a deposit. **All open swaps require a deposit of at
least 2 ADA.** While this behavior is due to the current protocol parameters, it is actually
a good thing since it helps prevents denial-of-service attacks from having a lot of "zombie swaps"
open (i.e., UTxOs where there is not enough of the offer asset for other users to swap).

It is possible to open swaps for multiple different trading pairs in a single transaction. The
minting policy is capable of still ensuring that all beacons are stored properly (with the proper
value and datum). For example, since each beacon's datum is specific to those beacons, mixing up
beacons and datums will cause the minting transaction to fail.

The `CreateSwap` allows also burning any beacons to enable composition with the `CloseOrUpdate`
redeemer. More on this later.


##### Closing or Updating a Swap

Open Swap UTxOs can be closed or updated by the address owner (signified by the address' staking
credential).

The `CloseOrUpdate` redeemer allows the owner to recover the deposit stored with the swap. **In
order to reclaim the deposit, the beacons must be burned.** As the redeemer name suggests, swaps
can also be updated inplace instead. The requirements for successfully using the `CloseOrUpdate`
redeemer are:

1) The beacons must go to an addres protected by the dApp validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    a) Exactly two kinds of beacons: pair beacon and offer beacon.
    b) The beacons must correspond to the beacons in the datum.
    c) There must be exactly 1 of each beacon.
    d) No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    a) `beaconId` == this policy id.
    b) `pairBeacon` == sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name)
    c) `offerId` == asset_id of the offer asset.
    d) `offerName` == asset_name of the offer asset.
    e) `offerBeacon` == sha2_256(offer_id ++ offer_name).
    f) `askId` == asset_id of the ask asset.
    g) `askName` == asset_name of the ask asset.
    k) `swapPrice` denominator > 0
    l) `swapPrice` > 0
5) The offer asset and ask assets must be different assets.
6) The address' staking credential must signal approval.

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need
to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must
be minted, the `CreateSwap` redeemer must be used. This behavior enables the swap owner to change
what trading pair a swap is for in a single transaction. This is instead of having to close the
original swap in one transaction only to re-open it in another transaction.

Requirement 6 guarantees that only the owner can close/update a swap and claim the assets.

##### Executing a Swap

Any Cardano user can execute an available swap using the `Swap` Redeemer as long as the swap
conditions are met.

At a high level, the `Swap` redeemer uses the swap input's output reference and datum to find the
corresponding swap output. The checks are essentially:
1) Does this output have the trading pair beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?
4) If "Yes", this is the corresponding output.

It then compares the value of that output to the input to determine the swap's asset flux. Since the
validator first checks for the trading pair beacon, each execution is dedicated for a specific
trading pair. Any other outputs are ignored in this specific execution. This logic works because a
script is executed once for every UTxO spent from the address. If input 1 is for beacon XYZ and
input 2 is for beacon ABC, the first execution can be dedicated to beacon XYZ and the second
execution can be dedicated to ABC. The net transaction will only succeed if all executions succeed.
This behavior allows cheaply composing swaps of different trading pairs that are located at the same
address. In other words, the design is taking advantage of the redundant executions.

At a low-level, for a swap execution to be successfull, all of the following must be true:

1. The input must contain the beacon for that trading pair - the required beacon is gotten from the
   datum for that UTxO. If the beacon is present, that means the datum can be trusted.
2. There must be an output to this address with the proper value and inline `SwapDatum`:
    - Must contain exactly the same value as the input except for the ask asset, offer asset, and
    ADA.
    - All fields in the datum must be the same as the input datum except the `prevInput` field must
    be `Just(input_ref)` where `input_ref` is the corresponding input's output reference.
3. Offered asset taken * price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. ADA can always be
deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner
and that swap inputs have a valid `price`: denominator > 0 and `price` > 0.

Requirements 2 & 4 guarantee that beacons from other trading pairs cannot be combined into one output
UTxO. This has two beneficial consequences:
1) The swap logic is kept very simple.
2) All swap UTxOs are as small as possible which maximizes how many swaps can be composed in a
single Tx as well as how easily the beacons can be queried.

Custom error messages are included to help troubleshoot why a swap failed.


#### Two-Sway Swaps

Two-way swaps use one universal validator script for all swap pairs and one universal minting script
for all trading pairs. Two-way swaps use three beacons: a trading pair beacon, an asset1 beacon, and
an asset2 beacon. The asset1 and asset2 beacons serve as offer beacons since two-way swaps can
technically go in either direction.

The asset1 beacon asset name is `sha2_256( asset1_policy_id ++ asset1_asset_name )`.

The asset2 beacon asset name is `sha2_256( asset2_policy_id ++ asset2_asset_name )`.

The trading pair beacon asset name is 
`sha2_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )`. However, in the case where one
of the assets is ADA (which is just the empty bytestring), the ADA policy id is set to "00" instead.
Without this changed, there would be no difference between the offer beacon and trading pair beacon
when ADA is part of a swap.

Asset1 and asset2 are the **sorted** trading pair. Trading pairs are sorted lexicographically by
name. For example, for the trading pair ADA <-> DJED, the empty bytestring (on-chain representation
for ADA's `CurrencySymbol`) is less than DJED's `CurrencySymbol` which means, for this trading pair,
asset1 is ADA and asset2 is DJED. This sorting is done for two reasons: 

1) The trading pair beacon asset name does not depend on the order of the swap's direction.
2) The datum information can be standardized for each trading pair, regardless of swap direction.

These beacons allow for expressive queries since it makes it possible to query all swaps where XYZ
is being offered, regardless of the asset being asked for. This is in addition to being able to
query all swaps for a specific trading pair. This extra expressiveness improves the arbitrager's
ability to find profitable paths through currently open swaps.

##### Swap Address

All users get a single swap address where all of their two-way swaps will be located. This universal
validator can enforce all possible swaps for the user. And since users only need to worry about a
single address, it keeps the UI simple.

##### Minting Redeemers

Two minting redeemers are introduced here, their usage is explained further below.

```Haskell
data BeaconRedeemer
  = CreateSwap 
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
  , pairBeacon :: TokenName -- ^ The asset name for the beacon for this trading pair.
  , asset1Id :: CurrencySymbol -- ^ The policy id for the first asset in the sorted pair.
  , asset1Name :: TokenName -- ^ The asset name for the first asset in the sorted pair.
  , asset1Beacon :: TokenName --^ The asset name for the asset1 beacon.
  , asset2Id :: CurrencySymbol -- ^ The policy id for the second asset in the sorted pair.
  , asset2Name :: TokenName -- ^ The asset name for the second asset in the sorted pair.
  , asset2Beacon :: TokenName --^ The asset name for the asset2 beacon.
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

Since all prices are askedAsset/offeredAsset, asset2 is being offered in `forwardPrice` and asset1
is being offered in `reversePrice`.

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets
in the UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for
DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same Tx. Since every
user explicitly defines their desired swap ratios, oracles are not required. The "global" price
naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of Lovelace.**

##### Creating a Two-Way Swap

Swap "writers" must first create a swap by using the `CreateSwap` Redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1) The beacons must go to an addres protected by the dApp validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    a) Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
    b) The beacons must correspond to the beacons in the datum.
    c) There must be exactly 1 of each beacon.
    d) The UTxO must have asset1 and/or asset2.
    e) No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    a) `beaconId` == this policy id.
    b) `pairBeacon` == sha2_256(asset1_id ++ asset1_name ++ asset2_id ++ asset2_name)
    c) `asset1Id` == asset_id of asset1 for that trading pair.
    d) `asset1Name` == asset_name of asset1 for that trading pair.
    e) `asset1Beacon` == sha2_256(asset1_id ++ asset1_name).
    f) `asset2Id` == asset_id asset2 for that trading pair.
    g) `asset2Name` == asset_name asset2 for that trading pair.
    h) `asset2Beacon` == sha2_256(asset2_id ++ asset2_name).
    i) `forwardPrice` denominator > 0
    j) `forwardPrice` > 0
    k) `reversePrice` denominator > 0
    l) `reversePrice` > 0
    m) asset1 < asset2

The validator will assume that the trading pairs are sorted which is why asset1 must be less than
asset2. Asset1 and asset2 cannot be the same asset.

Once the beacons are minted to the swap address, the spending script does not allow closing the swap
*unless* the beacons are being burned. This prevents beacons from being sent to unrelated addresses
and guarantees the integrity of the off-chain queries.

All swaps must be stored with the proper beacons. Due to this requirement, the absolute minimum
possible value for a swap UTxO to have is about 2 ADA. This minimum value can be reclaimed upon
closing the swap so it should be thought of as a deposit. **All open swaps require a deposit of at
least 2 ADA.** While this behavior is due to the current protocol parameters, it is actually a
good thing since it helps prevents denial-of-service attacks from having a lot of "zombie swaps"
open (i.e., UTxOs where there is not enough of the assets for other users to swap).

It is possible to open swaps for multiple different trading pairs in a single transaction. The
minting policy is capable of still ensuring that all beacons are stored properly (with the proper
value and datum). For example, since each beacon's datum is specific to that beacon, mixing up
beacons and datums will cause the minting transaction to fail.

The `CreateSwap` redeemer allows burning any beacons to enable composition with the `CloseOrUpdate`
redeemer. More on this later.


##### Closing or Updating a Swap

Open Swap UTxOs can be closed or updated by the address owner (signified by the address' staking
credential).

The `CloseOrUpdate` redeemer allows the owner to recover the deposit stored with the swap. **In
order to reclaim the deposit, the beacons must be burned.** As the redeemer name suggests, swaps
can also be updated inplace instead. The requirements for successfully using the `CloseOrUpdate`
redeemer are:

1) The beacons must go to an addres protected by the dApp validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    a) Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
    b) The beacons must correspond to the beacons in the datum.
    c) There must be exactly 1 of each beacon.
    d) The UTxO must have asset1 and/or asset2.
    e) No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    a) `beaconId` == this policy id.
    b) `pairBeacon` == sha2_256(asset1_id ++ asset1_name ++ asset2_id ++ asset2_name)
    c) `asset1Id` == asset_id of asset1 for that trading pair.
    d) `asset1Name` == asset_name of asset1 for that trading pair.
    e) `asset1Beacon` == sha2_256(asset1_id ++ asset1_name).
    f) `asset2Id` == asset_id asset2 for that trading pair.
    g) `asset2Name` == asset_name asset2 for that trading pair.
    h) `asset2Beacon` == sha2_256(asset2_id ++ asset2_name).
    i) `forwardPrice` denominator > 0
    j) `forwardPrice` > 0
    k) `reversePrice` denominator > 0
    l) `reversePrice` > 0
    m) asset1 < asset2
5) The address' staking credential must approve.

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need
to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must
be minted, the `CreateSwap` redeemer must be used. This behavior enables the swap owner to change
what trading pair a swap is for in a single transaction. This is instead of having to close the
original swap in one transaction only to re-open it in another transaction. 

##### Executing a Swap

Any Cardano user can execute an available swap using either the `ForwardSwap` or `ReverseSwap`
redeemer as long as the swap conditions are met. When `ForwardSwap` is used, the `forwardPrice` is
used and asset2 is the offer asset while asset1 is the ask asset. When `ReverseSwap` is used, the
`reversePrice` is used and asset1 is the offer asset and asset2 is the ask asset.

At a high level, the validator uses the swap input's output reference and datum to find the
corresponding swap output. The checks are essentially:
1) Does this output have the trading pair beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?
4) If "Yes", this is the corresponding output.

It then compares the value of that output to the input to determine the swap's asset flux. Since the
validator first checks for the beacon, each execution is dedicated for a specific trading pair. Any
other outputs are ignored in this specific execution. This logic works because a script is executed
once for every UTxO spent from the address. If input 1 is for beacon XYZ and input 2 is for beacon
ABC, the first execution can be dedicated to beacon XYZ and the second execution can be dedicated to
ABC. The net transaction will only succeed if all executions succeed. This behavior allows cheaply
composing swaps of different trading pairs that are located at the same address. In other words, the
logic takes advantage of the redundant executions.

At a low-level, for a swap execution to be successfull, all of the following must be true:

1. The input must contain the beacon for that trading pair - the required beacon is gotten from the
   datum for that UTxO. If the beacon is present, that means the datum can be trusted.
2. There must be an output to this address with the proper value and inline `SwapDatum`:
    - Must contain exactly the same value as the input except for the ask asset and offer asset.
    - All fields the same as the input datum except the `prevInput` field must be `Just(input_ref)`
      where `input_ref` is the corresponding input's output reference.
3. Offered asset taken * price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. ADA can always be
deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner
and that swap inputs have a valid price: denominator > 0 and price > 0.

Requirements 2 & 4 guarantee that beacons from other trading pairs cannot be combined into one output
UTxO. This has two beneficial consequences:

1) The swap logic is very simple.
2) All swap UTxOs are as small as possible which maximizes how many swaps can be composed in a
single Tx as well as how easily the beacons can be queried.

Custom error messages are included to help troubleshoot why a swap failed.


## Features Discussion

### Full Delegation Control

Since all users get their own DEX address, all users maintain full delegation and voting control of
their assets at all times. This means, as the blockchain economy grows, Cardano's Proof-of-Stake
becomes more secure. **This is exactly the opposite of a blockchain economy based on LPs.**

### Trustlessly Composable Swaps

Since multiple swaps are composable into a single transaction, any arbitrarily complex swap
transaction can be created. The only limits are the size and execution budgets of transactions,
which are Cardano protocol parameters.

The swaps can even be trustlessly composed with other p2p protocols. For example, Alice can use a
swap to convert DJED to USDC, use the USDC to buy an options contract on the secondary market, and
then immediately execute that contract, all in the same transaction. The transaction will fail if
any of the intermediate steps fail. As this example, this trustless composition allows for a healthy
and complex economy to form on Cardano - one with absolutely no required middle-men.

### Naturally Incentivized Liquidity

Liquidity in cardano-swaps is an *emergent* property; it arises from the (healthy) incentives for
users to provide liquidity with two-way swaps and arbitragers to compose complex swaps. As long as
the entry and exit swap pairs have enough liquidity, arbitragers can spread liquidity into less
liquid swap pairs. As a bonus, **the very nature of *illiquidity* implies great arbitrage
opportunities**. The more illiquid a swap pair, the greater the potential arbitrage profits.
Providing liquidity and participating in arbitrage/market-making is permissionless, so anyone can
create their own strategies/algorithms to provide liquidity or find the most profitable "path"
through the sea of available swaps.

Since stake pool operators are required to always be connected to the network, they are uniquely
positioned to serve as arbitragers. It provides another potential source profit for all stake pools,
including the small pool operator.

##### The Contrived Example

``` Txt
Alice has 10 ADA in her swap address and is willing to swap them for 0.5 AGIX/ADA.
Bob has 10 AGIX in his swap address and is willing to swap them for 1 ADA/AGIX.
```

In this example, Charlie can profitably arbitrage and fulfill both of these swaps like this:

``` Txt
Charlie looks up all swap addresses willing to swap AGIX/ADA. Charlie finds Alice's address.
Charlie looks up all swap addresses willing to swap ADA/AGIX. Charlie finds Bob's address.
Using Bob's reference script, Charlie gives Bob 10 ADA and receives 10 AGIX.
Using Alice's reference script, Charlie gives Alice 5 AGIX and receives 10 ADA.
Charlie now has his original 10 ADA plus an additional 5 AGIX.
This all occurs in one transaction where Charlie pays the transaction fee.
```

On net, Charlie pays the transaction fees and receives 5 AGIX in return, while both Alice and Bob's
swaps are fulfilled.

##### The Realistic Example

``` Txt
Alice has 10 ADA in her swap address and is willing to swap them for 1 DUST/ADA.
Bob has 10 DUST in his swap address and is willing to swap them for 0.5 AGIX/DUST.
Charlie has 10 AGIX in his swap address and is willing to swap them for 1 HOSKY/AGIX.
Mike has 10 HOSKY in his swap address and is willing to swap them for 1 ADA/HOSKY.
```

In this example, Sarah can profitably arbitrage and fulfill all of these swaps like this:

``` Txt
Sarah looks up all swap addresses willing to swap DUST/ADA. Sarah finds Alice's address.
Sarah looks up all swap addresses willing to swap AGIX/DUST. Sarah finds Bob's address.
Sarah looks up all swap addresses willing to swap HOSKY/AGIX. Sarah finds Charlie's address.
Sarah looks up all swap addresses willing to swap ADA/HOSKY. Sarah finds Mike's address.
Sarah gives Mike 10 ADA and receives 10 HOSKY.
Sarah gives Charlie 10 HOSKY and receives 10 AGIX.
Sarah gives Bob 5 AGIX and receives 10 DUST.
Sarah gives Alice 10 DUST and receives 10 ADA.
Sarah now has her original 10 ADA plus an additional 5 AGIX.
This all occurs in one transaction where Sarah pays the transaction fee.
```

On net, Sarah pays the transaction fees and receives 5 AGIX in return, while four swaps are
fulfilled. As shown in the this example, Sarah fulfills *both* the AGIX/DUST swap and the HOSKY/AGIX
swap by "passing through" those pairs on her way back to ADA. As long as the entry and exit pairs
(in this case ADA/HOSKY and DUST/ADA) have enough liquidity, arbitragers can spread that liquidity
into less liquid swap pairs. And since two-way swaps naturally incentivize providing liquidity for
major trading pairs, this requirement for entry and exit liquidity is naturally incentivized to be
satisfied.

### Expressive Beacon Queries

Offer based queries allow another class of queries. For example, arbitragers can use them find
profitable swap compositions based on current market demand. Without them, the arbitragers would be
required to know what trading pairs to check in advance. But what if there was a token that they had
never heard of? This opportunity would be missed. With offer based queries, these unheard of tokens
would be returned by the first query and the arbitrager algorithms can decide which other assets to
query based on those results. In short, offer based queries allow for organic discovery of
composition paths.

### Single Swap Addresses

This dramatically improves the usability of the DEX since users/frontends don't need to manage a
possibly infinite set of addresses for users.

### Democratic Upgradability

Upgrades to Cardano-Swaps can propagate through the ecosystem of users in a similarly democratic
fashion as SPOs upgrading their pools to a new version of `cardano-node`. Since users can close
their swaps at any time, whenever there is a potential upgrade, users can choose to close their
current swaps and recreate them with the new contracts. There is never a bifurcation of liquidity
due to the composable nature of all swaps, regardless of the version.

### Frontend Agnosticism

Thanks to the query-ability of beacon tokens, it is trivial for any frontend to integrate with
Cardano-Swaps. For example, any wallet can integrate `cardano-swaps` by adding support for querying
the beacon tokens. They can also add their own user friendly way to create and use swaps. The only
requirement is that all frontends/users agree to use the same beacon token standard. There is no
need for risky extensions or dedicated frontends.

## Benchmarks and Fee Estimations (YMMV)

The protocol is capable of handling up to 15 swaps in a single transaction, regardless of the
composition of one-way and two-way swaps in the transaction.

**No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found in the [Benchmarks](./Benchmarks/) folder.

## FAQ

### Most swaps will be just above or just below the market price so won't this mean the concurrency benefits are overblown?

Not at all. This is literally how a real market works. The market price is not a real phenomenon; it
is estimated based off the buyers' and sellers' asking prices. Look at any order book in TradFi and
you will see exactly this "gap" between the buyers and sellers. You can think of the market price as
the midpoint between the buyers' and sellers' asking prices. It is not an actual point.

A healthy market does not need optimal concurrency *at* the market price. Instead, what is important
is that when market sentiment shifts, the market price can easily shift with it. This is exactly
what is possible by having most swaps concentrated just above or below the market price. If Alice
suddenly finds herself really needing USDC, she will have ample swaps to choose from if she pays a
little more than she originally would have. Her new circumstances may justify this higher price.
This is literally what it means to say that market sentiment has shifted and the price needs to be
updated to reflect it.

### What about swap collisions?

Recall the composition example above. What would happen if another user simultaneously submits a
transaction using *one* of the same "Swap" UTxOs as an input? Whichever transaction is processed by
(most) nodes first will succeed, and the other will fail entirely. If the first transaction is
processed before the second (as in a normal scenario), the second transaction will fail due to the
UTxO being spent already. Either way, the collateral from the arbitragers will be safe. 

Arbitragers will be in constant competition with one another to query and execute the most
profitable swaps at any point in time, and must strike a balance between simple swaps and complex
swaps. There is a lot of stochasticity here; the higher the ratio of open swaps to arbitragers, the
lower the overall chances of a collision, but the more attractive a particular swap is, the higher
the chances of *that* particular collision. And "attractiveness" depends not only on the price of
one swap, but how that price relates to all possible compositions involving that swap. The fact
there there can potentially be an infinite number of paths through all open swaps means arbitragers
can profit without having to rely on the same paths.

### Won't all swaps be executed at the *worst* possible price the swap will support?

That is a very misleading way to put it. To be exact, there is no incentive for any user to pay more
than the required swap ratio. This question is usually in the context of comparing atomic swaps
against LPs. So let's compare the two! 

As already mentioned, there is no incentive for users to pay more than the required swap ratio.
However, **it is impossible to ever get less than the swap ratio**. So while your profits are
bounded, so are your loses.

For LPs, users will get whatever ratio the algorithm determines to be fair. When things are good,
this means users get the best possible price. However, when things are bad, this means users will
get the worst possible price. In other words, while profits are unbounded, so are the losses.

Which is more important for mass adoption: bounded losses or unbounded profits? The answer is
definitely bounded losses. Risk management is something that is required by all institutions. Most
people tend to to be loss averse ([prospect theory](https://en.wikipedia.org/wiki/Prospect_theory)).
A regulated institution (especially a publicly traded one) will need to always be able to manage any
risk of loss. Yield farming, where the institution is going to get paid a useless token, is simply
not going to cut it.

The fact that Cardano-Swaps prioritizes risk management over unbounded profits is a feature, not a
bug.

### If all users share a spending script, how are their assets protected?

The spending script gets the staking credential from the address of the UTxO being spent at
run-time. When an owner related action is being performed (closing or updating positions), the
spending script requires that the staking credential "signals approval" of the action:

- If the staking credential is a pubkey, the staking pubkey must sign the transaction.
- If the staking credential is a script, the script must be executed in the same transaction.

The staking credential effectively *becomes* the "owner" for all actions except for the actual swap
execution, in which case the spending credential is used directly.

It is possible to execute a staking script even if 0 ADA is withdrawn from a reward address. The
only requirement to use staking scripts like this is that the associated stake address must be
registered and delegated. Stake addresses can be utilized this way as soon as the
registration+delegation transaction is added to the chain. There is no epoch waiting-period.

### If Cardano-Swaps reaches mass adoption, won't TVL on Cardano go down?

Yes. Yes it will. TVL is a silly metric. It is a measure of who can be most inefficient with DeFi
capital.

### Won't Cardano-Swaps become obsolete once Axo launches?

No, it will not. Cardano-Swaps and Axo are going after two very different use cases.

Cardano-Swaps is trying to be the RSS feed for DeFi: uncensorable and equal access for all. This
niche prioritizes availability over throughput. Meanwhile, Axo is trying to target the professional
trader which prioritizes throughput over availability. They are both very legitimate, albiet very
different niches.

Cardano-Swaps existence is not meant to compete with Axo. Instead, it is meant to keep Axo in check.
If Axo knows that users cannot go anywhere else, it could treat its users as citizens instead of
customers. By Cardano-Swaps being an unalienable alternative (even if it is less performant), Axo is
forced to *always* treat its users as customers. The moment Axo starts abusing its users, the users
can leave and use Cardano-Swaps. It is no different than transitioning from banking out of necessity
(where profits are privatized and losses are socialized) to banking only if it adds value to your
life.

## Conclusion

The Cardano-Swaps protocol has all of the desired properties of a highly composable p2p-DEX that can
serve as the bedrock of a vibrant and complex blockchain economy. Thanks to the use of Beacon
Tokens, decentralization is no longer limited by the design of DEXs. Instead, the limiting factor is
now the off-chain querying. However, innovations in this space are still in the early days. The
Koios API is an example of a more decentralized off-chain platform. As the technology improves, so
too will the decentralization of the protocol.
