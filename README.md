# Cardano-Swaps

A [p2p-DeFi protocol](https://github.com/zhekson1/CSL-DeFi-Protocols) for trustlessly swapping
fungible tokens on the Cardano Settlement Layer. All users maintain full custody, delegation
control, and voting control of their assets at all times. No batchers are required.

> **Note** Knowledge of basic Haskell syntax and `cardano-cli` usage is recommended. For a list of
> everything that has changed from the previous version, see the [Changelog](CHANGELOG.md).

The Getting Started instructions can be found [here](./GettingStarted.md) and the benchmarks can
be found [here](./Benchmarks/).

---
## Table of Contents 
- [Abstract](#abstract)
- [Motivation](#motivation)
- [Cardano-Swaps](#cardano-swaps)
- [Specification](#specification)
- [Benchmarks and Fee Estimations](#benchmarks-and-fee-estimations-ymmv)
- [Features Discussion](#features-discussion)
- [FAQ](#faq)
- [Conclusion](#conclusion)

## Abstract

Cardano-Swaps is a p2p-DeFi protocol for swapping fungible tokens on the Cardano Settlement Layer
(CSL). It solves many of the pitfalls of current DEX implementations by empowering users to deploy
their own (and interact with each others') script addresses. This leads to the formation of an
order-book style "distributed" DEX. The protocol supports two kinds of swaps: one-way swaps and
two-way swaps. One-way swaps emulate the basic limit order. Meanwhile, two-way swaps provide a
mechanism for naturally incentivizing users to provide liquidity to the DEX without having to rely
on yield farming. Furthermore, since swaps can be freely composed, arbitragers are naturally
incentivized to spread the available liquidity across all trading pairs. This protocol is natively
composable with all DApps.

## Motivation

Many DEXs on Cardano are currently implemented in ways that lock users' assets into a tightly fixed,
and/or centrally maintained, set of script addresses and UTxOs. This *concentrated DApp* design
pattern is reminiscent of Ethereum's accounts-based programming paradigm.

However, the concentrated DApp design does not map well to eUTxO based blockchains. To be exact, a
major consequence of pooling assets into a predefined set of UTxOs, which are typically referred to
as liquidity pools (LPs), is that the DEX has a significant concurrency bottleneck since all users
must share the one-time-use UTxOs. The ultimate result of this bottleneck is that users are not
allowed to directly interact with the DEX. Instead, users are required to go through (usually
permissioned) middle-men called batchers. 

As an alternative to LP based DEXs, order-book style DEXs give every user their own UTxO with their
own terms. This approach lends itself much better to eUTxO based blockchains. However, this design
currently suffers from similar availability challenges due to the difficulting of finding and
matching open orders. In order to workaround this limitation, order-book style DEXs also tend to
resort to the concentrated DApp design since it is easier to find orders if they are located at a
single address (or a few addresses) than if they were scattered across many addresses. Furthermore,
and more importantly, current order-book style DEXs suffer from a lack of liquidity due to not
having a way to incentive users to provide liquidity to the DEX. This lack of early liquidity is why
most DEXs currently use the LP based DEX architecture. 

This result is unfortunate since LP based DEXs are entirely unsuitable to be used as the foundation
of a trustless and decentralized economy, especially those built on Proof-of-Stake (PoS)
blockchains:

##### Why the LP based architecture is not the right architecture for DEXs (in no particular order)

- No matter how performant a system of batchers is, their resources do *not* scale in proportion to
the number of users unless new batchers can permissionlessly join when demand is high. 
- Since an economy's health is proportional to how easily and accurately price discovery can occur,
DEXs that do not allow users to freely express their own desired prices can only result in an
unhealthy blockchain economy. Furthermore, impermanent loss (a consequence of not being able to
express desired prices) makes it impossible for users to properly manage risk.
- Batcher based protocols lack trustless composability with other DApps; a critical feature for both
scaling eUTxO-based DeFi and building a fully featured, trustless blockchain economy.
- The concentration of assets into a single address or few addresses directly undermines the
security assumptions of PoS blockchains. 
- The concentration of assets forces users to give up voting rights over their assets. In essence,
users are forced to pick between participating in DeFi and participating in decentralized
governance. Furthermore, whichever entity controls the voting power of the concentrated assets has
tremendous influence over the decentralized government.

The last two bullets especially mean it is *impossible* for LPs to serve as the foundation of a
trustless and decentralized economy.

##### Why the workarounds for the issues of the LP based architecture are not sufficient

- Impermanent loss compensation through yield farming - yield farming is ultimately printing a
useless token to make Alice whole for being forced to sell at a price she otherwise would not have
sold. To put it bluntly, Alice lost $100 of a stablecoin and was given some economically useless
token as compensation. And while the yield tokens are being minted, the value of the already useless
token can only decrease over time. Without the yield tokens, there is literally no incentive to
provide liquidity to LPs so if yield farming ever stopped, the DEX would collapse. Yield farming is
a currency crisis waiting to happen. Professional institutions cannot comply with regulations and
manage risk in this context. In short, mass adoption of LP based DEXs by professional institutions
is impossible.
- No way for users to fully express their own desired prices - LPs base the exchange rates on supply
and demand of the assets in the LP. But this is wholly insufficient to properly reflect the true
market sentiment. To quote [investopedia](https://www.investopedia.com/terms/p/pricediscovery.asp),
"The process of price discovery looks at a number of tangible and intangible factors, including
supply and demand, investor risk attitudes, and the overall economic and geopolitical environment.
Simply put, it is where a buyer and a seller agree on a price and a transaction occurs." There is
fundamentally no way for algorithms that are based solely on supply and demand to properly reflect
the true market sentiment. And when market sentiment cannot be accurately reflected, misallocation
of resources is inevitable (ie, an unhealthy economy). Fractionalizing LPs does not solve this issue
because they can never be fractionalized enough to match the expressiveness of each user specifying
their own prices - these wouldn't be LPs anymore. This level of expressiveness is only possible with
order-book DEXs.
- Batchers can always take advantage of their privileged position as middle-men - miner extractable
value (MEV) will always be an issue as long as the use of middle-men is *required*. Permissioned
batchers exacerbate this issue since only the "chosen few" can even have this unique opportunity to
rip off users.
- Batcher based protocols cannot trustlessly compose - since LPs require going through middle-men,
the ultimate transaction for Alice will likely not even be seen by Alice prior to submission to the
blockchain. How can Alice express that she wants her swap composed with an options contract and
expect it to be trustlessly executed? How can Bob also do this? Finally, how can the smart contracts
enforce that *both* Alice's and Bob's compositions are properly executed when their requests are
batched together? This is not an issue of standards; this is an issue of whether or not the required
composition logic can even fit in a transaction where other DApp logic is being executed. The only
way for the composition to be trustless is if smart contracts enforce it. To put it concretely, in
Alice's composition, the swap logic and the options logic must both be executed in the same
transaction where the composition logic is executed. Given the very small amount of execution units
available to scripts in a transaction, is there even room for this extra logic? The composition
logic requires two things: it must support *arbitrary* compositions and it must be extremely cheap
since the logic of the actual DApps being composed must also be executed in the same transaction.
*p2p protocols do not have the same limitations since Alice personally creates and signs her own
transaction.* The above two requirements are only requirements when middle-men are creating and
executing transactions on the behalf of users. Even if some composition logic could fit in the
transaction, would it actually support arbitrary composition (as opposed to only a few priviledged
DApps being supported)? Even if the answer to this question is yes, that extra logic being executed
means less space for other DApps to compose in the transaction. Therefore, protocols that require
batchers will never be able to offer the the same level of trustless composition as fully p2p
protocols. The only way to actually match the level of composition of p2p protocols is for the
batcher based composition to *not be trustless* (without the composition logic, more DApps can fit
in a transaction).
- No delegation control or voting control while using the DEX - DEXs try to issue governance tokens
to get around this but fair distribution of the governance tokens is rarely accomplished (it is a
very hard problem). Even if the governance tokens were distributed fairly, they are largely just for
show since the DEX creators can still choose to go against the vote. There is no trustless
connection between the governance tokens and what actually happens with the stake/DApp. Finally,
even if there was a trustless connection, there is no way to actually have all users express their
own stake preferences when the stake is decided democratically. It is almost inevitable that the
voting outcome will go against the wishes of some users. *This is in direct violation of the
security assumptions of PoS.* No matter how you look at it, LP based DApps are an existenstial
problem for PoS blockchains. As LP based DApps become more adopted, the underlying PoS blockchain
will only become more centralized. LPs can never be used as the foundation for a trustless and
decentralized PoS blockchain economy.

DEXs that hope to become the foundation for a healthy and fully trustless, decentralized economy
must adhere to a radically different approach that takes full advantage of the composability and
availability guarantees offered by the eUTxO model while not undermining the security of the very
blockchain the economy is built on.

## The Cardano-Swaps Protocol

Cardano-Swaps is an order-book based DEX that takes inspiration from Axo's programmable swaps
[design](https://www.axo.trade/whitepaper.pdf), adds delegation control as a foundational feature,
naturally incentivizes its own liquidity, and, through the use of Beacon Tokens, removes the need for
specialized indexers. In short, it solves all of the pressing issues that made order-book DEXs
inferior to LP based DEXs. 

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
This has major implications that will be discussed in the Features Discussion section.

Since the swaps are freely composable, arbitrarily complex swaps can be created. For example, Alice
can chain ADA -> ERGO with ERGO -> DUST to create a transaction that converts ADA -> DUST. Not only
does this composition allow for one-to-many conversions in a single transaction (eq, ADA to ERGO and
DJED), but it also opens up the possibility for arbitragers to find profitable arbitrage
opportunities. This potential for profit naturally incentivizes liquidity to be spread across all
trading pairs instead of it being siloed into one trading pair as in TradFi and LP based DEXs.

Finally, through the use of beacon tokens, the protocol is fully p2p. No batchers are required
although they can be used if desired. And thanks to beacon tokens, users can get their own addresses
which guarantees users maintain full custody, delegation, and voting control of their assets at all
times.

The only remaining challenge is the querying capacity of existing off-chain APIs, such as Blockfrost
or Koios. (This is not a limitation for users with powerful enough hardware, as they can run their
own API database).

## Specification

Each type of swap is comprised of a validator / beacon policy pair. This means that each type of
swap gets its own universal address. For example, all of Alice's one-way swaps, regardless of the
trading pair, will be located at address X and all all of Alice's two-way swaps, regardless of the
trading pair, will be located at address Y. Address X != address Y. Furthermore, since each type
of swap has its own beacon policy, each type is easily distinguishable off-chain while still being
easily queryable by end users.

This modular approach of having a separate script pair for each swap makes it easy to add new kinds
of swaps in the future.

:exclamation: While the spec shows one-way swap datums and redeemers having the same names as the
two-way swap datums and redeemers *they are distinct data types*. Imagine they are in separate name
spaces.

### One-Sway Swaps

One-way swaps use one universal validator script for all swap pairs, and one universal minting
policy for all one-way beacons. There are three beacons for one-way swaps: the trading pair beacon,
the offer beacon, and the ask beacon.

The offer beacon asset name is `sha2_256( "01" ++ offer_policy_id ++ offer_asset_name )`.

The ask beacon asset name is `sha2_256( "02" ++ offer_policy_id ++ offer_asset_name )`.

The trading pair beacon asset name is `sha2_256( offer_id ++ offer_name ++ ask_id ++ ask_name )`.
However, in the case where one of the assets is ada (which is just the empty bytestring), the ada
policy id is set to "00" instead. For example, an ADA -> DJED swap pair beacon would be:
`sha2_256(djed_id ++ djed_name ++ "00" ++ "")`. Without this change, each direction would not get
its own beacon (ie, DJED -> ADA would have the same trading pair beacon as ADA -> DJED).

The hashes are used because token names are capped at 64 characters. Simply concatenating the name
information would not fit within this requirement. However, hashing the result of the concatenation
guarantees all names are unique while still fitting within 64 characters.

These beacons allow for expressive queries since they allow querying by offer, by ask, or by trading
pair. This extra expressiveness improves the arbitrager's ability to find profitable paths through
currently open swaps.

##### Swap Address

All users get a single swap address where all of their one-way swaps will be located. This universal
validator can enforce all possible swaps for the user. And since users only need to worry about a
single address, it keeps the UI simple.

##### Minting Redeemers

Two minting redeemers are introduced here; their usage is explained further below.

```Haskell
data BeaconRedeemer
  = CreateSwap 
  | BurnBeacons
```

##### Validator Redeemers

Two validator redeemers are introduced here; their usage is explained further below.

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
  { beaconId :: CurrencySymbol -- ^ Beacon policy id for one-way swaps.
  , pairBeacon :: TokenName -- ^ Trading pair beacon asset name for this swap.
  , offerId :: CurrencySymbol -- ^ Offer policy id for this swap.
  , offerName :: TokenName -- ^ Offer asset name for this swap.
  , offerBeacon :: TokenName -- ^ Offer beacon asset name for this swap.
  , askId :: CurrencySymbol -- ^ Ask policy id for this swap.
  , askName :: TokenName -- ^ Ask asset name for this swap.
  , askBeacon :: TokenName -- ^ Ask beacon name for this swap.
  , swapPrice :: Rational -- ^ The desired swap ratio: Ask/Offer.
  , prevInput :: Maybe TxOutRef -- ^ The output reference for the corresponding swap input.
  }
```

The beacon information in the datum prevents misuse of beacons. The validator forces all beacons to
be burned instead of being withdrawn. This ensures that beacons can only ever be located at swap
addresses. **If the wrong beacon information is supplied to the datum, the resulting Swap UTxO can
be locked forever.** (See the FAQ for more information on this.)

The `Rational` type is a fraction (decimal types do not work on-chain). All prices in Cardano-Swaps
are relative (similar to limit orders in an order-book exchange). Swaps are always priced in
askedAsset/offeredAsset. For example, if ada is being offered for dust at a price of 1.5
(converted to 3/2), the contract requires that 3 dust are deposited for every 2 ada removed from
the swap address. Ratios of DUST:ADA >= 3/2 will pass, while ratios < 3/2 will fail. 

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets in
the Swap UTxO must be swapped.** For example, if there is 100 ada in a swap address requesting 2:1
for dust, a user may swap 20 ada, as long as they return 80 ada and 10 dust in the same Tx. Since
every user explicitly defines their desired swap ratios, oracles are not required. The "global"
price naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of lovelace.**

##### Creating a One-Way Swap

Swap "writers" must first create a swap by using the `CreateSwap` redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1) The beacons must go to an addres protected by the one-way swap validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    - Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
    - The beacons must correspond to the beacons in the datum.
    - There must be exactly 1 of each beacon.
    - No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    - `beaconId` == this policy id.
    - `pairBeacon` == pair beacon asset name for this swap.
    - `offerId` == policy id of the offer asset.
    - `offerName` == asset name of the offer asset.
    - `offerBeacon` == offer beacon asset name for this swap's offer asset.
    - `askId` == policy id of the ask asset.
    - `askName` == asset name of the ask asset.
    - `askBeacon` == ask beacon asset name for this swap's ask asset.
    - `swapPrice` denominator > 0
    - `swapPrice` > 0
5) The offer asset and ask assets must be different assets.

Once the beacons are minted to the swap address, the spending script does not allow closing the swap
*unless* the beacons are being burned. This prevents beacons from being sent to unrelated addresses
and guarantees the integrity of the off-chain queries. 

All swaps must be stored with the proper beacons. Due to this requirement, the absolute minimum
possible value for a swap UTxO to have is about 2 ada. This minimum value can be reclaimed upon
closing the swap so it should be thought of as a deposit. **All open swaps require a deposit of at
least 2 ada.** 

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

1) The beacons must go to an addres protected by the one-way swap validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    - Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
    - The beacons must correspond to the beacons in the datum.
    - There must be exactly 1 of each beacon.
    - No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    - `beaconId` == this policy id.
    - `pairBeacon` == pair beacon asset name for this swap.
    - `offerId` == policy id of the offer asset.
    - `offerName` == asset name of the offer asset.
    - `offerBeacon` == offer beacon asset name for this swap's offer asset.
    - `askId` == policy id of the ask asset.
    - `askName` == asset name of the ask asset.
    - `askBeacon` == ask beacon asset name for this swap's ask asset.
    - `swapPrice` denominator > 0
    - `swapPrice` > 0
5) The offer asset and ask assets must be different assets.
6) The address' staking credential must signal approval.
7) Any unused beacons must be burned.

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need
to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must
be minted, the `CreateSwap` redeemer must be used. This behavior enables the swap owner to change
what trading pair a swap is for in a single transaction. This is instead of having to close the
original swap in one transaction only to re-open it in another transaction.

Requirement 6 guarantees that only the owner can close/update a swap and claim the assets.

##### Executing a Swap

Any Cardano user can execute an available swap using the `Swap` redeemer as long as the swap
conditions are met.

At a high level, the `Swap` redeemer uses the swap input's output reference and datum to find the
corresponding swap output. The checks are essentially:
1) Does this output have the trading pair beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?
4) If "Yes" to (3), this is the corresponding output.

It then compares the value of that output to the input to determine the swap's asset flux. Since the
validator first checks for the trading pair beacon, each execution is dedicated to a specific
trading pair. Any other outputs are ignored in this specific execution. This logic works because a
script is executed once for every UTxO spent from the address. If input 1 is for beacon XYZ and
input 2 is for beacon ABC, the first execution can be dedicated to beacon XYZ and the second
execution can be dedicated to ABC. The net transaction will only succeed if all executions succeed.
This behavior allows cheaply composing swaps of different trading pairs that are located at the same
address. In other words, the design is taking advantage of the redundant executions.

At a low-level, for a swap execution to be successfull, all of the following must be true:

1. The input must contain the beacon for that trading pair - the required beacon is gotten from the
   datum for that UTxO. If the beacon is present, that means the datum is valid.
2. There must be an output to this address with the proper value and inline `SwapDatum`:
    - Must contain exactly the same value as the input except for the ask asset, offer asset, and
    ada.
    - All fields in the datum must be the same as the input datum except the `prevInput` field must
    be `Just(input_ref)` where `input_ref` is the corresponding input's output reference.
3. Offered asset taken * price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. Ada can always be
deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner
and that swap inputs have a valid `price`: denominator > 0 and `price` > 0.

Requirements 2 & 4 guarantee that beacons from other trading pairs cannot be combined into one output
UTxO. This has two beneficial consequences:
1) The swap logic is kept very simple.
2) All swap UTxOs are as small as possible which maximizes how many swaps can be composed in a
single Tx as well as how easily the beacons can be queried.

Custom error messages are included to help troubleshoot why a swap failed.


### Two-Sway Swaps

Two-way swaps use one universal validator script for all swap pairs and one universal minting script
for all trading pairs. Two-way swaps use three beacons: a trading pair beacon, an asset1 beacon, and
an asset2 beacon. The asset1 and asset2 beacons serve as both offer and ask beacons since two-way
swaps can technically go in either direction.

The asset1 beacon asset name is `sha2_256( asset1_policy_id ++ asset1_asset_name )`.

The asset2 beacon asset name is `sha2_256( asset2_policy_id ++ asset2_asset_name )`.

The trading pair beacon asset name is `sha2_256( asset1_id ++ asset1_name ++ asset2_id ++
asset2_name )`. However, in the case where one of the assets is ada (which is just the empty
bytestring), the ada policy id is set to "00" instead. For example, an ADA <-> DJED swap pair beacon
would be: `sha2_256("00" ++ "" ++ djed_id ++ djed_name)`. This is required to distinguish
between the trading pair and asset beacons whenever ada is part of the pair.

Asset1 and asset2 are the **sorted** trading pair. Trading pairs are sorted lexicographically by
name. As in the previous example, for the trading pair ADA <-> DJED, the empty bytestring (on-chain
representation for ada's policy id) is less than djed's policy id which means, for this trading
pair, asset1 is ada and asset2 is djed. This sorting is done for two reasons: 

1) The trading pair beacon asset name does not depend on the swap's direction.
2) The datum information can be standardized for each trading pair, regardless of swap direction.
This makes processing the off-chain queries more efficient.

The hashes are used because token names are capped at 64 characters. Simply concatenating the name
information would not fit within this requirement. However, hashing the result of the concatenation
guarantees all names are unique while still fitting within 64 characters.

These beacons allow for expressive queries since they allow querying by offer, by ask, or by trading
pair. This extra expressiveness improves the arbitrager's ability to find profitable paths through
currently open swaps.

##### Swap Address

All users get a single swap address where all of their two-way swaps will be located. This universal
validator can enforce all possible swaps for the user. And since users only need to worry about a
single address, it keeps the UI simple.

##### Minting Redeemers

Two minting redeemers are introduced here; their usage is explained further below.

```Haskell
data BeaconRedeemer
  = CreateSwap 
  | BurnBeacons
```

##### Validator Redeemers

Three validator redeemers are introduced here; their usage is explained further below.

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
be burned instead of being withdrawn. This ensures that beacons can only ever be located at swap
addresses. **If the wrong beacon information is supplied to the datum, the resulting Swap UTxO can
be locked forever.** (See the FAQ for more information on this.)

The `Rational` type is a fraction (decimal types do not work on-chain). All prices in Cardano-Swaps
are relative (similar to limit orders in an order-book exchange). Swaps are always priced in
askedAsset/offeredAsset. For example, if ada is being offered for dust at a price of 1.5 (converted
to 3/2), the contract requires that 3 dust are deposited for every 2 ada removed from the swap
address. Ratios of DUST:ADA >= 3/2 will pass, while ratios < 3/2 will fail.

Since all prices are askedAsset/offeredAsset, asset2 is being offered in `forwardPrice` and asset1
is being offered in `reversePrice`.

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets
in the UTxO must be swapped.** For example, if there is 100 ada in a swap address requesting 2:1 for
dust, a user may swap 20 ada, as long as they return 80 ada and 10 dust in the same Tx. Since every
user explicitly defines their desired swap ratios, oracles are not required. The "global" price
naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of lovelace.**

##### Creating a Two-Way Swap

Swap "writers" must first create a swap by using the `CreateSwap` redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1) The beacons must go to an addres protected by the two-way swap validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    - Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
    - The beacons must correspond to the beacons in the datum.
    - There must be exactly 1 of each beacon.
    - No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    - `beaconId` == this policy id.
    - `pairBeacon` == trading pair beacon asset name for this swap.
    - `asset1Id` == policy id of asset1 for that trading pair.
    - `asset1Name` == asset name of asset1 for that trading pair.
    - `asset1Beacon` == beacon asset name for asset1.
    - `asset2Id` == policy id of asset2 for that trading pair.
    - `asset2Name` == asset name of asset2 for that trading pair.
    - `asset2Beacon` == beacon asset name for asset2.
    - `forwardPrice` denominator > 0
    - `forwardPrice` > 0
    - `reversePrice` denominator > 0
    - `reversePrice` > 0
    - asset1 < asset2

The validator will assume that the trading pairs are sorted which is why asset1 must be less than
asset2. Asset1 and asset2 cannot be the same asset.

Once the beacons are minted to the swap address, the spending script does not allow closing the swap
*unless* the beacons are being burned. This prevents beacons from being sent to unrelated addresses
and guarantees the integrity of the off-chain queries. 

All swaps must be stored with the proper beacons. Due to this requirement, the absolute minimum
possible value for a swap UTxO to have is about 2 ada. This minimum value can be reclaimed upon
closing the swap so it should be thought of as a deposit. **All open swaps require a deposit of at
least 2 ada.** 

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

1) The beacons must go to an addres protected by the two-way swap validator script.
2) The beacons must go to an address using a valid staking credential.
3) The UTxOs with the beacons must have the proper value:
    - Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
    - The beacons must correspond to the beacons in the datum.
    - There must be exactly 1 of each beacon.
    - No extraneous assets are in the UTxO. ADA is always allowed.
4) The beacons must be stored with the proper inline `SwapDatum`:
    - `beaconId` == this policy id.
    - `pairBeacon` == trading pair beacon asset name for this swap.
    - `asset1Id` == policy id of asset1 for that trading pair.
    - `asset1Name` == asset name of asset1 for that trading pair.
    - `asset1Beacon` == beacon asset name for asset1.
    - `asset2Id` == policy id of asset2 for that trading pair.
    - `asset2Name` == asset name of asset2 for that trading pair.
    - `asset2Beacon` == beacon asset name for asset2.
    - `forwardPrice` denominator > 0
    - `forwardPrice` > 0
    - `reversePrice` denominator > 0
    - `reversePrice` > 0
    - asset1 < asset2
5) The address' staking credential must approve.
6) Any unused beacons must be burned.

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need
to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must
be minted, the `CreateSwap` redeemer must be used. This behavior enables the swap owner to change
what trading pair a swap is for in a single transaction. This is instead of having to close the
original swap in one transaction only to re-open it in another transaction. 

Requirement 5 guarantees only the swap owner can close/update swaps.

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
4) If "Yes" to (3), this is the corresponding output.

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
   datum for that UTxO. If the beacon is present, that means the datum is valid.
2. There must be an output to this address with the proper value and inline `SwapDatum`:
    - Must contain exactly the same value as the input except for the ask asset, offer asset, and
    ada.
    - All fields the same as the input datum except the `prevInput` field must be `Just(input_ref)`
      where `input_ref` is the corresponding input's output reference.
3. Offered asset taken * price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. Ada can always be
deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner
and that swap inputs have a valid price: denominator > 0 and price > 0.

Requirements 2 & 4 guarantee that beacons from other trading pairs cannot be combined into one output
UTxO. This has two beneficial consequences:

1) The swap logic is very simple.
2) All swap UTxOs are as small as possible which maximizes how many swaps can be composed in a
single Tx as well as how easily the beacons can be queried.

Custom error messages are included to help troubleshoot why a swap failed.


## Benchmarks and Fee Estimations (YMMV)

The protocol is capable of handling 11-14 swaps in a single transaction, regardless of the
composition of one-way and two-way swaps in the transaction.

**No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found in the [Benchmarks](./Benchmarks/) folder.


## Features Discussion

### Full Custody

Since users get their own DEX addresses which use their own staking credentials for spending
authorization, users maintain full custody of their assets at all times while using the DEX.
*Not your keys, not your crypto.*

### Full Delegation & Voting Control

Since all users get their own DEX address, all users maintain full delegation and voting control of
their assets at all times. This means, as the blockchain economy grows, Cardano's Proof-of-Stake
becomes more secure. **This is exactly the opposite of a blockchain economy based on LPs.**

### Trustlessly Composable Swaps

Since multiple swaps are composable into a single transaction, any arbitrarily complex swap
transaction can be created. The only limits are the size and execution budgets of transactions,
which are Cardano protocol parameters.

The swaps can be trustlessly composed with any DApp (not just the p2p protocols), including those
that use batchers. For example, Alice can use a swap to convert djed to usdc, use the usdc to buy an
options contract on the secondary market, and then immediately execute that contract, all in the
same transaction. The transaction will fail if any of the intermediate steps fail. No meta-logic is
required to enforce trustless composition. As this example shows, this trustless composition allows
for a complex economy to form on Cardano - one with absolutely no required middle-men. And since
this trustless composition across protocols is something even TradFi doesn't have (ie, only the Wall
Streets of the world have access to these compositions), the blockchain economy can possibly allow
for even greater economic flexibility than what is currently possible in TradFi.

### Naturally Incentivized Liquidity

Liquidity in Cardano-Swaps is a naturally *emergent* property; it arises from the (healthy)
incentives for users to provide liquidity with two-way swaps and arbitragers to compose complex
swaps. As long as the entry and exit swap pairs have enough liquidity, arbitragers can spread
liquidity into less liquid swap pairs. As a bonus, **the very nature of *illiquidity* implies great
arbitrage opportunities**. The more illiquid a swap pair, the greater the potential arbitrage
profits. Providing liquidity and participating in arbitrage is permissionless, so anyone can create
their own strategies/algorithms to provide liquidity or find the most profitable "path" through the
sea of available swaps.

Since stake pool operators are required to always be connected to the network, they are uniquely
positioned to serve as arbitragers. It can provide another potential source of profit for all stake
pools operators, including the small pool operator who rarely makes blocks.

To fully appreciate the implications of the two-way swaps, it helps to consider a few examples:

##### The Contrived Arbitrage Example

``` Txt
Alice has 10 ada in her swap address and is willing to swap them for 0.5 AGIX/ADA.
Bob has 10 agix in his swap address and is willing to swap them for 1 ADA/AGIX.
```

In this example, Charlie can profitably arbitrage and fulfill both of these swaps like this:

``` Txt
Charlie looks up all swap addresses willing to swap AGIX/ADA. Charlie finds Alice's address.
Charlie looks up all swap addresses willing to swap ADA/AGIX. Charlie finds Bob's address.
Charlie gives Bob 10 ada and receives 10 agix.
Charlie gives Alice 5 agix and receives 10 ada.
Charlie now has his original 10 ada plus an additional 5 agix.
This all occurs in one transaction where Charlie pays the transaction fee.
```

On net, Charlie pays the transaction fees and receives 5 AGIX in return, while both Alice's and
Bob's swaps are fulfilled.

##### The Realistic Arbitrage Example

``` Txt
Alice has 10 ada in her swap address and is willing to swap them for 1 DUST/ADA.
Bob has 10 dust in his swap address and is willing to swap them for 0.5 AGIX/DUST.
Charlie has 10 agix in his swap address and is willing to swap them for 1 HOSKY/AGIX.
Mike has 10 hosky in his swap address and is willing to swap them for 1 ADA/HOSKY.
```

In this example, Sarah can profitably arbitrage and fulfill all of these swaps like this:

``` Txt
Sarah looks up all swap addresses willing to swap DUST/ADA. Sarah finds Alice's address.
Sarah looks up all swap addresses willing to swap AGIX/DUST. Sarah finds Bob's address.
Sarah looks up all swap addresses willing to swap HOSKY/AGIX. Sarah finds Charlie's address.
Sarah looks up all swap addresses willing to swap ADA/HOSKY. Sarah finds Mike's address.
Sarah gives Mike 10 ada and receives 10 hosky.
Sarah gives Charlie 10 hosky and receives 10 agix.
Sarah gives Bob 5 agix and receives 10 dust.
Sarah gives Alice 10 dust and receives 10 ada.
Sarah now has her original 10 ada plus an additional 5 agix.
This all occurs in one transaction where Sarah pays the transaction fee.
```

On net, Sarah pays the transaction fee and receives 5 agix in return, while four swaps are
fulfilled. As shown in this example, Sarah fulfills *both* the AGIX/DUST swap and the HOSKY/AGIX
swap by "passing through" those pairs on her way back to ada. As long as the entry and exit pairs
(in this case ADA/HOSKY and DUST/ADA) have enough liquidity, arbitragers can spread that liquidity
into less liquid swap pairs. And since two-way swaps naturally incentivize providing liquidity for
major trading pairs, this requirement for entry and exit liquidity is naturally incentivized to be
satisfied.

Since the current design is capable of handling 11-14 swaps in a single transaction, there is ample
flexibility for finding arbitrage opportunities. For example, maybe the 3rd swap is at a slight loss
but the 5th swap is such a good arbitrage that it more than makes up for it resulting in a net
profit for the arbitrage.

##### The Stablecoin <-> Stablecoin Two-Way Swap

Currently, DApps treat stablecoins individually (ie, you may be able to borrow a loan in DJED but
not USDC). This means, even though all stablecoins are practically the US dollar, the liquidity is
fractured across all stablecoins. Two-way swaps change this.

Imagine if Charlie has a swap for DJED <-> USDC where converting assets in either direction requires
paying a 1% premium. The very existence of this swap, combined with the fact that Cardano-Swaps is
freely composable with all DApps, means users can freely convert between stablecoins as needed. The
liquidity would no longer be fractured. Alice could convert her DJED to USDC to buy an options
contract that is asking for USDC. She just has to pay Charlie the 1% fee for the conversion.

Since two-way swaps can go in either direction, these swaps do not need to be reset after each swap.
For example, after Alice deposits DJED into the swap to claim USDC, Bob can now use the swap to
convert his USDC to DJED by paying a 1% fee. Either way, Charlie makes 1% per swap and *is paid
directly in the stablecoin being deposited*. And since the stablecoins are supposed to be 1:1 with
the US dollar, the only risk is a peg-break. Anyone can be a liquidity provider and profit like
Charlie does in this example.

Being able to treat all stablecoins as a single stablecoin will be a game changer for DeFi.

##### The Volatile Two-Way Swap

While the stablecoin <-> stablecoin two-way swaps are the least risky, there is also an incentive to
provide liquidity for volatile pairs. Consider two assets like ada and dust. Imagine if Alice thinks
ada is going to appreciate against dust; therefore, she would rather have ada. Her prices can
reflect this: converting ADA -> DUST could require a 15% premium while converting DUST -> ADA could
require only a 5% premium. Would anyone pay the 15% premium?

Yes, arbitragers would. Imagine if an arbitrager found a really good opportunity but they needed the
ada. As long as the overall profit from the arbitrage can cover the 15% premium (and then some), the
arbitrager may be more than willing to pay this high premium. Considering that the protocol supports
chaining up to 14 swaps and illiquidity implies great arbitrage opportunities, it is actually likely
that arbitragers will be able to find opportunities that justify paying this high premium. For
example, let's say a single arbitrage yields a 5% profit and, since 14 swaps can fit in a single
transaction, 5 of these arbitrages can fit in a single transaction. That means this transaction
would yield 25% in revenue for the arbitrager. What if the arbitrager could not execute this
transaction without the two-way ADA -> DUST swap? Should the arbitrager skip the opportunity?
Definitely not! Even after paying the 15% premium, the arbitrager would have a 10% profit on this
single transaction. It is definitely worth it for the arbitrager to pay the premium.

Consider the realistic arbitrage example where Sarah paid the transaction fee and got 5 agix from
the arbitrage. According to the benchmarks, composing 4 swaps requires about a 0.5 ada transaction
fee. This is practically equivalent to Sarah paying 0.5 ada for 5 agix. What if the current market
rate is 0.66 ADA/AGIX? If Sarah immediately sold her new agix, she would get 3.3 ada. That is a 560%
yield! Sarah has plenty of income to cover a 15% premium if needed. 

**Providing liquidity for volatile pairs is an active trade.** If the price moved against Alice by
more than 15%, Alice's ada will likely be entirely converted to dust. Alice must keep an eye on the
exchange rates to protect herself. Higher potential profit also carries with it higher potential
risk. 

This use case is meant more for professional traders but anyone can provide liquidity like this.


### Expressive Beacon Queries

Offer and ask based queries allow another class of queries, in addition to querying by trading pair.
For example, arbitragers can use these queries to find profitable swap compositions based on current
market demand. Without them, the arbitragers would be required to know what trading pairs to check
in advance. But what if there was a token that they had never heard of? This opportunity would be
missed. With offer and ask based queries, these unheard of tokens would be returned by the first
query and the arbitrage algorithms can decide which other assets to query based on those results. In
short, offer and ask based queries allow for organic discovery of arbitrage composition paths.

### Single Swap Addresses

This dramatically improves the usability of the DEX since users/frontends don't need to manage a
possibly infinite set of addresses for users.

### Democratic Upgradability

Upgrades to Cardano-Swaps can propagate through the ecosystem of users in a similarly democratic
fashion as SPOs upgrading their pools to a new version of `cardano-node`. Since users can close
their swaps at any time, whenever there is a potential upgrade, users can choose to close their
current swaps and recreate them with the new contracts. There is never a bifurcation of liquidity
due to the composable nature of all swaps, regardless of the protocol's version.

### Frontend Agnosticism

Thanks to the query-ability of beacon tokens, it is trivial for any frontend to integrate with
Cardano-Swaps. For example, any wallet can integrate `cardano-swaps` by adding support for querying
the beacon tokens. They can also add their own user friendly way to create and use swaps. The only
requirement is that all frontends/users agree to use the same beacon token standard. There is no
need for dedicated (censorable) frontends.

### Naturally Resistant to Denial-of-Service Attacks

Recall that each swap requires a deposit of at least 2 ada. While this behavior is due to the
current protocol parameters, it is actually a good thing since it helps prevents denial-of-service
attacks. For example, since the protocol does not check that the assets in the datum are real
assets, it is possible to create swaps for fake assets. *This does not impact normal users who are
querying swaps for a known trading pair.* The user would have to deliberately query a trading pair
that contains a fake asset in order to see these fake UTxOs.

These fake asset UTxOs only really impact offer and ask queries which are mainly for arbitragers who
will likely have powerful hardware. Since these fake UTxOs still require 2 ada, creating millions of
them (enough to disrupt the arbitragers) would require a deposit of millions of ada throughout the
life of the denial-of-service attack. The only way to reclaim the ada is to stop the attack.
Furthermore, checking if an asset is real is trivial (look for mint history). Once an arbitrager
realizes an asset is fake, they can ignore it. So an actual denial-of-service attack would require
constantly changing the millions of fake swaps to new fake swaps; this has a large cost in terms of
transaction fees which makes this impractical to do at scale.


## FAQ

### Most swaps will be just above or just below the market price so won't this mean the concurrency benefits are overblown?

Not at all. This is literally how a real market works. The market price is not a real phenomenon; it
is estimated based off the buyers' and sellers' asking prices. Look at any order book in TradFi and
you will see exactly this "gap" between the buyers and sellers. You can think of the market price as
the midpoint between the buyers' and sellers' asking prices. It is not an actual point.

A healthy market does not need optimal concurrency *at* the market price. Instead, what is important
is that when market sentiment shifts, the market price can easily shift with it. This is exactly
what is possible by having most swaps concentrated just above or below the market price. If Alice
suddenly finds herself really needing usdc, she will have ample swaps to choose from if she pays a
little more than she originally would have. Her new circumstances may justify this higher price.
This is literally what it means to say that market sentiment has shifted and the market price needs
to be updated to reflect it.

### What about swap collisions?

Recall the composition example above. What would happen if another user simultaneously submits a
transaction using *one* of the same "Swap" UTxOs as an input? Whichever transaction is processed by
(most) nodes first will succeed, and the other will fail entirely. If the first transaction is
processed before the second (as in a normal scenario), the second transaction will fail due to the
UTxO being spent already. Either way, the collateral from the transactions will be safe. 

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
However, **it is impossible to ever get less than the specified amount**. So while your profits are
bounded, so are your losses.

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

The spending script gets the staking credential from the address of the Swap UTxO being spent at
run-time. *The address of the UTxO being validated is given to the script by the ledger which means
the address is unspoofable.* When an owner related action is being performed (closing or updating
positions), the spending script requires that the staking credential "signals approval" of the
action:

- If the staking credential is a pubkey, the staking pubkey must sign the transaction.
- If the staking credential is a script, the script must be executed in the same transaction.

The staking credential effectively *becomes* the "owner" for all actions except for the actual swap
execution, in which case the spending credential is used directly.

> Note: It is possible to execute a staking script even if 0 ada is withdrawn from a reward address.
> The only requirement to use staking scripts like this is that the associated stake address must be
> registered and delegated. Stake addresses can be utilized this way as soon as the
> registration+delegation transaction is added to the chain. There is no epoch waiting-period.

### What happens if a Swap UTxO is created at an address without a staking credential?

Just like how stake pool operators cannot deny delegators, addresses cannot deny UTxO creation.
Therefore, it is possible to create UTxOs at a DEX address that does not have a staking credential.
This address effectively has no "admin". In order to prevent permanent locking, the protocol allows
anyone to spend this UTxO; it is first come, first serve.

There does not seem to be a use case for this address which is why beacons cannot be minted to an
address without a staking credential.

### What if a Swap UTxO has a misconfigured datum?

Swap UTxOs with misconfigured datums can never be stored with the beacons so other users will never
see them. However, depending on how the datum was misconfigured, the UTxO could be locked forever.

Since the spending script does not know which assets are the beacons, it has no choice but to trust
the datum. If the datum "lies" to the script, the script will not behave differently; it will treat
that asset as the beacon even though it isn't one. This means the script will demand that the asset
is burned instead of withdrawn. Imagine what would happen if ada is specified as the beacon. It is
not possible to burn ada which means this burning requirement is impossible to satisfy. This UTxO
would be locked forever. The same could be true for any native asset that the user does not control
the burning of.

*Creating a misconfigured datum is very hard to do.* The `cardano-swaps` CLI handles creating the
datum for the user. Messing up the datum would require manually creating the datum which requires
deliberate intention. There is no reason for users to ever have to manually create the datums. Any
frontend that integrate Cardano-Swaps should also handle the datum creation for the user.

### Is it possible to have a Swap UTxO with beacons and none of the swappable asset?

Yes, however this is not an issue. Querying the beacons returns all swaps for that trading pair and
the results can be further filtered by whether or not they have the swappable asset. Koios allows
doing this extra filtering server-side so the computational burden is not placed on the end-user.
The `cardano-swaps` CLI uses Koios like this. The following query is an example that returns all
UTxOs with the beacon (policy id == `5c84...`, asset name == `6424...`) but only if it also contains
the offer asset (policy id == `c0f8...`, asset name == `4f74...`):

```Bash
curl -g -X POST -H "content-type: application/json" 'https://preprod.koios.rest/api/v1/asset_utxos?select=is_spent,asset_list&is_spent=eq.false&asset_list=cs.[{"policy_id":"c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","asset_name":"4f74686572546f6b656e0a"}]' -d '{"_asset_list":[ ["5c84e7433fdd13ca161bd4a6cd68fc6462fcca5aea07c5692be091df","6f245d4be7333223d88e7211c4de116208b8a3898c9a196e0579391fd03c8860"] ], "_extended": true }'
```

### If Cardano-Swaps reaches mass adoption, won't TVL on Cardano go down?

Yes. Yes it will. TVL is a silly metric. It is a measure of who can be most inefficient with DeFi
capital.

### Won't Cardano-Swaps become obsolete once Axo launches?

No, it will not. Cardano-Swaps and Axo are going after two very different use cases.

Cardano-Swaps is trying to be an unalienable financial utility: uncensorable and equal access for
all. This niche prioritizes availability over throughput. Meanwhile, Axo is trying to target the
professional trader which prioritizes throughput over availability. They are both very legitimate,
albiet very different niches.

Cardano-Swaps' existence is not meant to compete with Axo. Instead, it is meant to keep Axo in
check. If Axo knows that users cannot go anywhere else, it could potentially abuse its users without
recourse. By Cardano-Swaps being an unalienable alternative (even if it is less performant), Axo is
forced to *always* treat its users well. The moment Axo starts abusing its users, the users can
leave and use Cardano-Swaps. It is no different than transitioning from banking out of necessity
(where profits are privatized and losses are socialized) to banking only if it adds value to your
life.

## Conclusion

The Cardano-Swaps protocol has all of the desired properties of a highly composable p2p-DEX that can
serve as the bedrock of a healthy, complex, trustless, and decentralized blockchain economy. Thanks
to the use of Beacon Tokens, decentralization is no longer limited by the design of DEXs. Instead,
the limiting factor is now the off-chain querying. However, innovations in this space are still in
the early days. The Koios API is an example of a more decentralized off-chain platform. As the
technology improves, so too will the decentralization of the protocol.
