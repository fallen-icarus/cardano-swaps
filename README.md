# Cardano-Swaps

:warning: Knowledge of basic Haskell syntax and cardano-cli usage is assumed.

The Getting Started instructions can be found [here](GettingStarted.md).

---
## Table of Contents 
- [Abstract](#abstract)
- [Motivation](#motivation)
- [Preliminary Discussion](#preliminary-discussion)
  - [Current DEX Deficiencies](#current-dex-deficiencies)
  - [Programmable Swaps](#programmable-swaps)
  - [Beacon Tokens](#beacon-tokens)
  - [The Cardano-Swaps Protocol](#the-cardano-swaps-protocol)
- [Specification](#specification)
  - [Personal Contracts](#personal-contracts)
  - [The DEX's Inline Datum](#the-dexs-inline-datum)
  - [Swap Contract Logic](#swap-contract-logic)
    - [Close Redeemer](#close-redeemer)
    - [Update Redeemer](#update-redeemer)
    - [Swap Redeemer](#swap-redeemer)
- [Features Discussion](#features-discussion)
- [Benchmarks and Fee Estimations](#benchmarks-and-fee-estimations-ymmv)
- [FAQ](#faq)
- [Conclusion](#conclusion)

---
## Abstract
`cardano-swaps` is a proof-of-concept implementation of an *organically* scalable DEX protocol for the Cardano Settlement Layer (CSL). It solves many of the pitfalls of current DEX implementations by empowering users to deploy their own (and interact with each others') script addresses. By doing so, users always maintain spending *and* delegation control of their assets, and can elect if/when to upgrade their addresses to new contract standards.


## Motivation
Many DEXes on Cardano are currently implemented in ways that lock users' assets into a tightly fixed, and/or centrally maintained, set(s) of script addresses. Such design patterns are reminiscent of the EVM's accounts-based programming paradigm, and inherit many of the same downsides; scalability bottlenecks and asset/stake centralization. DEXes that hope to achieve massive scale on the CSL must adhere to a radically different approach that takes full advantage of the concurrency and parallelism offered by the eUTxO model. `cardano-swaps` is a first attempt at such an approach. 


## Preliminary Discussion
To appreciate the necessity of new DEX standards, it first important to understand the deficiencies of the current status-quo:

### Current DEX Deficiencies  
One consequence of centralized script addresses is the necessity for liquidity pools and LP providers as discrete entities. LPs are a common feature of many DEXes, coming with undesirable properties and corresponding sets of workaround "solutions". However, these workarounds have issues themselves, as explored here:

| Undesirable Property | Workaround "Solution" | Issues |
| :--: | :--: | :--: |
| Impermanent Loss | Yield Farming & Concentrated Liquidity | Medium - Long term unsustainability |
| Incomplete or No Delegation Control | <ul><li>Asset pool fractionalization</li><li>Indirect Delegation via Governance Tokens</li></ul> | <ul><li>Unfair distribution of Governance tokens </li><li>Unavoidable centralization of stake (Major issue for Ouroboros)</li></ul>
| Scaling Bottlenecks | Batchers, Execution/Routing Engines, and/or other middlemen | Middlemen can take advantage of their position between users and the protocol. Even if MEV is mitigated, more users --> more execution demand --> possible centralization of middlemen if the batchers are not permissionless |

Of course, this is not an exhaustive list, and even if some workarounds can be somewhat effective, the underlying design *principles* are suboptimal.

The more decoupled delegation control is from the owner, the more distorted Ouroboros' game theory becomes. It is difficult to predict the extent of this distortion, so minimizing it is of critical importance. Additionally, current implementations of order-book style DEXes (which don't use LPs) still suffer from the scalability challenges of permissioned batchers. No matter how performant a system of batchers is, their resources do **not** scale in proportion to the number of users unless new batchers can permissionlessly join when there is high demand.

All of this is to say that, much like Bittorrent and the CSL-CCL stack, the best p2p protocols are ones that scale in *proportion* to the number of users. DEXes are no different.

Cardano-Swaps achieves batcher/router-free scalability *with* delegation control via a novel combination of user-controlled script addresses and Beacon Tokens (both are expanded upon below).

### Programmable Swaps
First proposed by Axo in their original [whitepaper](https://www.axo.trade/whitepaper.pdf), programmable swaps are a more promising design choice than liquidity pools. Swaps are simply UTxOs that may be consumed if and only if the resulting TX outputs satisfy the input script's logic. Swaps can be fragmented across many user-controlled addresses, so delegation control is maintained. Users then execute swaps from each other's addresses. Since each swap is atomic and explicitly defined, in aggregate they are the optimal expression of (intra)market sentiment. This design pattern scales naturally, since there must be at *least* as many swap addresses as there are users. 

The challenge now becomes one of indexing: how do users differentiate each others' swap addresses from all other addresses on Cardano, *without* relying on a specialized indexer/router? This is where Beacon Tokens come into play.

### Beacon Tokens
Beacon Tokens are a (WIP) native token standard that "tag" on-chain data in a way that is efficiently queryable by off-chain APIs.  They enable cardano-swaps users to designate their script addresses as "swappable", such that they stand out in sea of other addresses. DDOS/bloat prevention is achieved by carefully marrying Beacons' minting policies with scripts' spending policies. This is expanded upon in the [Specification section](#specification) below.

:white_check_mark: The novel use of *Beacon Tokens* for "tagging" on-chain data can be generalized for many dApps, not just DEXes. More on this in the [Beacon Token CIP](https://github.com/cardano-foundation/CIPs/pull/466).

Putting this all together, we finally have:

### The Cardano-Swaps Protocol
Cardano-Swaps takes inspiration from Axo's programmable swaps design, adds delegation control as a foundational feature, and, through the use of Beacon Tokens, removes the need for specialized indexers. The only remaining bottleneck is the querying capacity of existing off-chain APIs, such as Blockfrost or Koios. (For users with powerful enough hardware, even this is not a limitation, as they can run their own API database).

Here are some of the key features of Cardano-Swaps:

  1. **Full Custody** - users always maintain full spending *and* delegation control over their assets.
  2. **Natural Concurrency** - throughput scales *with* the number of users. No specialized batchers/indexers required.
  3. **Composable Swaps** - many swaps can be fulfilled in a single transaction by "chaining" swap logic
  4. **Emergent Liquidity** - arbitragers are incentivized to spread liquidity to all trading pairs
  5. **Zero Slippage** - minimum swap prices are explicitly defined
  6. **No Superfluous "DEX" Tokens** - ADA is all you need to pay for TX fees.
  7. **Democratic Upgradability** - users choose if/when to use new contracts.
  8. **Frontend Agnosticism** - relatively straightforward integration with existing frontends (i.e. wallets)

Some of these features are explained further in the [Discussion section](<#Discussion & FAQ>) below


## Specification

### Personal Contracts
Cardano addresses are made up of both a payment credential and a staking credential. As long as the staking credential is unique to the user, delegation control over the address is maintained. Cardano-swaps leverages this duality by giving users addresses that are composed of the same spending scripts (per swap pair) and unique staking credentials. The spending credential is implemented in a way that gives the staking credential authority over all owner related actions, besides the actual swap execution. 

:heavy_exclamation_mark: To force the use of a staking credential, it is not possible to mint a beacon token to an address without a staking credential.

### Minting and Using Beacon Tokens
Beacon Tokens are used to tag `cardano-swaps` addresses so they are readily queryable via an off-chain API, such as Koios or Blockfrost. It is relatively straightforward to find all addresses that contain a specific native token. Here are some examples:

| Task | Koios Api | Blockfrost Api |
|--|--|--|
| Addresses with a beacon | [api](https://api.koios.rest/#get-/asset_address_list) | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1addresses/get)|
| UTxOs at the address | [api](https://api.koios.rest/#post-/address_info) | [api](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos/get)|

The UTxO query also returns which UTxOs contain reference scripts, allowing users to execute their swaps.

Technically, all native tokens can be used as beacons like this but this feature is usually not the intended one. The name *Beacon Token* refers to any native token whose only purpose is to act as a tag/beacon.

#### Beacon Tokens with Cardano-Swaps
Every trading pair gets its own spending script. This is accomplished with the help of an extra parameter. Here is the data type of that extra parameter:

``` Haskell
data SwapConfig = SwapConfig
  { swapOffer :: (CurrencySymbol,TokenName)
  , swapAsk :: (CurrencySymbol,TokenName)
  }
```

**Each combination of `swapOffer` and `swapAsk` results in a different spending script. All spending scripts are identical except for this extra parameter. The hash of the resulting spending script is then used as an extra parameter to the beacon policy. *All beacon policies are identical except for this extra parameter.* As a result, every `SwapConfig` will also have its own unique beacon policy.**

Since the beacon policy-id itself carries all the information needed to determine which trading pair is being used, every beacon uses the empty token name - this is enforced by the minting policy.

#### Minting Requirements
Minting beacons for `cardano-swaps` is a tightly controlled process. In order to mint beacons, **all of the following must be true**:

1. Only one beacon is minted per Tx.
2. The minted beacon uses an empty token name.
3. The beacon is minted to an address protected by the `cardano-swaps` spending script for a particular trading pair.
4. The beacon is minted to an address with a staking credential (either a pubkey or a script).
5. The beacon is stored in the UTxO containing the reference script of the spending script for that trading pair.
6. The datum of the output containing the beacon must have the proper beacon symbol.

Once the beacon is minted to the swap address, the spending script does not allow consuming the beacon's UTxO *unless* the beacon is being burned. This is done to prevent beacons from being sent to unrelated addresses.

#### Burning Requirements
Since minting and spending beacons are so heavily controlled, there is no reason to regulate burning. Burning is always allowed as long as the burn redeemer is only used to burn beacons.

#### Querying the Beacon Tokens
Below is an example response from querying the beacon tokens:
``` JSON
[
  {
    "assets": [
      {
        "asset": "lovelace",
        "quantity": 150000000
      }
    ],
    "price_denominator": 1,
    "price_numerator": 1,
    "swap_address": "addr_test1xqc8dluz63hw3z5jf38nj8vc8hr6w3zffqype75rwzhfqtmgzy4wvf0pv6q4z499a70zm6sadjzwc0w6xx622s2fx30qsq8ppj",
    "swap_ref_script_id": "325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0",
    "utxo_id": "325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#1"
  }
]
```

Only one UTxO was found, containing only Lovelace (ADA). Notice how this UTxO's required reference script ID was also returned. This response has everything a user needs to swap with the address.

#### Generalizing Beacon Tokens
While these beacons are used to "tag" all necessary information for engaging in swaps (reference script UTxOs and available swap UTxOs), they can be used for tagging ANY on-chain data w.r.t. to:

1. The address containing the Beacon
2. The UTxO containing the Beacon 
3. The information for any transaction(s) the Beacon has ever been in 

For example, it is possible to query the metadata of all transactions the Beacon was ever a part of. This can be used to create an easily queryable & trustless "metadata history" trail. Here are some examples:

| | Blockfrost | Koios |
| :--: | :--: | :--: |
| TX history | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1transactions/get) | [api](https://api.koios.rest/#get-/asset_txs) |
| TX metadata | [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1metadata/get)| [api](https://api.koios.rest/#post-/tx_metadata) |

Beacon Tokens make all of this information readily queryable; no configuration of the tokens is necessary. This can be used in a wide variety of dApps - **the only requirement is that the beacon token is unique for each *kind* of datatype.**

---
### The DEX's Inline Datum
For users to see each others' asking prices, all datums for the DEX must be inline datums. `cardano-swaps` contracts enforce this behavior whenever possible. Here is the datum type:

``` Haskell
-- | Swap Datum
type Price = Rational -- ^ askedAsset/offeredAsset
data SwapDatum = SwapDatum
  {
    swapPrice :: Price,
    swapBeacon :: Maybe CurrencySymbol -- ^ Policy id for the DEX's beacon token
  }
```

#### swapPrice
The `Rational` type is a fraction (decimal types do not work properly). Fortunately, there is no loss of functionality from using fractions. Users are able to supply decimals to `cardano-swaps` and it will properly convert the decimal to `Rational`.

All prices in Cardano-Swaps are local (similar to limit orders in an order-book exchange). The price is always askedAsset/offeredAsset. For example, if $ADA is being offered for $DUST at a price of 1.5 (converted to 3/2), the contract requires that 3 $DUST are deposited for every 2 $ADA removed from the swap address. Ratios < 3/2 will fail, while ratios >= 3/2 will pass. 

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets at the swap address or UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same TX.

Since every user explicitly defines their desired swap ratios, oracles are not required. The "global" price naturally emerges where the local bids and asks meet - just like an order-book.

:heavy_exclamation_mark: A zero or negative price means the assets are effectively free. A malicious user may deposit a UTxO with a negative price in the datum in order to steal user funds. To prevent this, swaps will fail unless all prices are greater than 0.

:heavy_exclamation_mark: For user-friendliness, when ADA is part of the trading pair, the price **MUST** be in units of ADA. The swap contract will convert to Lovelace when necessary. If Lovelace units are entered, conversion will still take place and produce undesired behavior.

#### swapBeacon
The `swapBeacon` field for the datum prevents misuse of beacons. The contract forces all assets with the supplied policy-id to be burned instead of being withdrawn. This ensures the beacons can never be found in an address unrelated to `cardano-swaps`. **If the wrong policy id is supplied, assets will be locked forever.** Only the UTxO containing the beacon needs to use `Just beaconSym`; all active swaps can use `Nothing` for this field. `cardano-swaps` CLI handles this part of the datum automatically, preventing accidental locking.

---
### Swap Contract Logic
Swap contracts have three possible actions, a.k.a. redeemers:

1. `Close` - withdraw any UTxO located at the swap address and burn the beacon
2. `Update` - update the asking price for UTxOs at the swap address
3. `Swap` - executing a swap with assets at the swap address

Only the owner (signified by the address' staking credential) is allowed to use the `Close` or `Update` redeemers. Anyone can use the `Swap` redeemer.

#### `Close` Redeemer
The `Close` redeemer allows the owner (signified by the address' staking credential) to recover the deposit stored with the reference script, and make the address undiscoverable by burning the beacon. **In order to reclaim the deposit, the beacon must be burned.** The requirements for successfully using the `Close` redeemer are:

1. All beacons among Tx inputs must be burned.
2. The staking credential must signal approval:
    - pubkey must sign
    - script must be executed in the same tx
3. Any new outputs to the address must contain the proper datum:
    - `swapPrice` > 0
    - `swapBeacon` == Nothing

#### `Update` Redeemer
The `Update` redeemer allows the owner to change the asking price of their position(s) by changing the inline datum attached of associated UTxOs. This action includes checks to ensure the new datum is properly used. The requirements for a successful update are:

1. No beacons among tx inputs.
2. The staking credential must signal approval:
    - pubkey must sign
    - script must be executed in the same tx
3. All new outputs to the address must contain the proper datum:
    - `swapPrice` > 0
    - `swapBeacon` == Nothing

The first requirement also means that the datum attached to the beacon's reference script *cannot* be updated. This is fine since that datum is never allowed in swaps.

#### `Swap` Redeemer
The `Swap` redeemer checks all of the assets leaving the swap address and all of the assets entering the swap address. For a successful swap, all of the following must be true:

1. No swap input utxos have reference scripts - this also protects the beacon.
2. All swap input prices are > 0.
3. All outputs to the swap address contain the proper datum:
    - `swapPrice` == weighted avg price of all swap inputs
    - `swapBeacon` == Nothing
4. Only the offered asset (as defined in `SwapConfig`) is leaving the swap address.
5. QuantityOfferedAssetTaken * weighted average price <= quantityAskedAssetGiven

Custom error messages are included to help troubleshoot why a swap failed. The weighted average price must match exactly what the swap contract calculates. To help with this, `cardano-swaps` can calculate the weighted price for you. The function `cardano-swaps` uses is the same function the on-chain swap contract uses.


## Features Discussion
Here are some of the most impactful features of Cardano-Swaps:

### Delegation Control
Since each user has their own swap address, the process of delegation is identical to that of a normal address (either by staking key or staking script). 

Check out the [delegation section](GettingStarted.md#delegate-the-swap-address) of [GettingStarted.md](GettingStarted.md) for an example using a staking pubkey.

### Composable Atomic Swaps
Since multiple swaps are combinable into a single transaction, any arbitrarily complex swap transaction can be created. The only limit is the size of the transaction itself.

Do you want to convert 10 ADA into 5 DUST and 5 AGIX? No problem! This can be done in one transaction.
What about converting 10 ADA, 5 DUST, and 3 WMT into 16 AGIX and 1 of your favorite NFTs? Piece of cake!

By composing these swaps in one transaction, many-to-many multi-asset swaps are possible. The only limits are the maximum transaction limits for Cardano.

### Emergent Liquidity
Liquidity in cardano-swaps is an *emergent* property; it arises from the (healthy) incentive for arbitragers to engage in complex swap transactions. Discrete liquidity pools are unnecessary.

#### The Contrived Example

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

On net, Charlie pays the transaction fees and receives 5 AGIX in return, while both Alice and Bob's swaps are fulfilled.

#### The Realistic Example

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

On net, Sarah pays the transaction fees and receives 5 AGIX in return, while four swaps are fulfilled.

#### Liquidity naturally flows to the less liquid pairs
As shown in the realistic example, Sarah fulfills *both* the AGIX/DUST swap and the HOSKY/AGIX swap by "passing through" those pairs on her way back to ADA. As long as the entry and exit pairs (in this case ADA/HOSKY and DUST/ADA) have enough liquidity, arbitragers can spread that liquidity into less liquid swap pairs.

As a bonus, **the very nature of *illiquidity* implies great arbitrage opportunities**. The more illiquid a swap pair, the greater the potential arbitrage profits. Participating in arbitrage is permissionless, so anyone can design their own algorithms for finding the most profitable "path" through the currently available swaps.

### Democratic Upgradability
Upgrades to `cardano-swaps` can propagate through the ecosystem of users in a similarly democratic fashion as SPOs upgrading their pools to a new version of `cardano-node`. Since users can close their swaps at any time, whenever there is a potential upgrade, users can choose to close their current swaps and recreate them with the new contracts. The main challenge here is the bifurcation of liquidity that occurs during upgrade periods. However, the more users there are and the more overall liquidity there is, the more this issue is minimized.

### Frontend Agnosticism
Thanks to the query-ability of beacon tokens, it is trivial for any frontend to integrate with Cardano-Swaps. For example, any wallet can integrate `cardano-swaps` by adding support for querying the beacon tokens. They can also add their own user friendly way to create and use swaps. The only requirement is that all frontends/users agree to use the same beacon token standard. There is no need for risky extensions or dedicated frontends.


## Benchmarks and Fee Estimations (YMMV)
Basic benchmarking tests were done to determine the maximum number of inputs or outputs that the spending script can handle in one transaction.

Currently, Plutus is limited in that a spending script is always executed once for *every* script input, **even if the inputs come from the same script address.** So if there are 5 inputs from one of the script addresses, the spending script will be executed 5 times. **The latter 4 are completely redundant in this situation.** These redundant executions impact the maximum number of inputs and outputs that can fit in one transaction. There is a Cardano Problem Statement (CPS) looking to address this ([here](https://github.com/cardano-foundation/CIPs/pull/418)). Nonetheless, basic benchmarking results are shown here:

### Creating a Live Address
For opening a new address, I was successfully able to mint the beacon, store the reference script, and create 80+ new swap positions in one transaction.

| Number of New Swaps | Tx Fee |
|:--:|:--:|
| 5 | 0.647711 ADA |
| 10 | 0.697339 ADA |
| 15 | 0.747768 ADA |
| 20 | 0.797797 ADA |
| 25 | 0.847869 ADA |

### Closing a Live Address
For closing a new address, I was successfully able to burn the beacon, remove the reference script, and close 8 open positions in one transaction.

| Number of Closed Swaps | Tx Fee |
|:--:|:--:|
| 1 | 0.490071 ADA |
| 2 | 0.574382 ADA |
| 3 | 0.674041 ADA |
| 4 | 0.789050 ADA |
| 5 | 0.919407 ADA |
| 6 | 1.065113 ADA |
| 7 | 1.226169 ADA |
| 8 | 1.402573 ADA |

### Updating Open Swaps
For updating open swaps, I was successfully able to update 5 positions and recreate them in one transaction. If you consolidate your open positions into one output, you may be able to update more in one transaction.

| Number of Swaps Updated | Tx Fee |
|:--:|:--:|
| 1 | 0.250309 ADA |
| 2 | 0.381332 ADA |
| 3 | 0.574544 ADA |
| 4 | 0.829945 ADA |
| 5 | 1.147533 ADA |

### Swapping Assets
For swapping assets, I was successfully able to chain together 4 swap utxos before the redundant executions caused the transaction to exceed the memory limit.

| Number of Swaps Chained | Tx Fee |
|:--:|:--:|
| 1 | 0.313006 ADA |
| 2 | 0.511202 ADA |
| 3 | 0.772815 ADA |
| 4 | 1.114271 ADA |



## FAQ

#### If all users share a spending script, how are their assets protected?
The spending script gets the staking credential from the address of the UTxO being spent at run-time. When an owner related action is being performed (closing or updating positions), the spending script requires that the staking credential "signals approval" of the action:

- If the staking credential is a pubkey, then the staking pubkey must sign the transaction.
- If the staking credential is a script, then the script must be executed in the same transaction.
- the staking credential effectively *becomes* the "owner" for all actions except for the actual swap execution, in which case the spending credential is used directly.

 It is possible to execute a staking script even if 0 ADA is withdrawn from a reward address. The only requirement to use staking scripts like this is that the associated stake address must be registered and delegated. Stake addresses can be utilized this way as soon as the registration+delegation transaction is added to the chain. There is no epoch waiting-period.

#### Why not just give each user their own spending script?
`cardano-swaps` relies on the usage of [beacon tokens](<#specification/##Beacon Tokens>) to "tag" addresses, which demarcates them as *distinct* for efficient off-chain querying/aggregation. However, **address distinction is impossible if each script address is composed of completely unique credentials.** In other words, for Beacon Tokens to work properly, there must be a clear distinction between addresses participating in a given dApp and all other addresses. `cardano-swaps` leverages the dual payment-staking credentials of Cardano addresses to maintain address-distinction (shared spending scripts) without sacrificing self-custody (unique staking keys/scripts). For further clarification, refer to the [Beacon Token CIP](https://github.com/cardano-foundation/CIPs/pull/466)

:pushpin: v1.0.0 of cardano-swaps used unique spending scripts; you can read about the limitations in the v1.0.0 commit README.

#### If all users have direct access to the spending script for that trading pair, why are reference scripts used?
To disincentivize beacon bloat. 

The reason is to incentivize users to actually use the DEX as intended. If there were no deposits, users can create a live address with a beacon token and then just leave it open even if they aren't actually using the address anymore (like if there are no swappable assets in it). This "zombie" address will still appear when querying the beacon tokens even though other users can't actually do anything with it. By requiring the deposits, users are incentivized to close unused addresses in order to get the deposits back.

#### What if two arbitragers compete for the same swap?
Recall the contrived example above. What would happen if Charlie and Mike try to arbitrage it at the same time?

1. Charlie and Mike both build their transactions since the UTxOs still exist.
2. Charlie and Mike submit their transaction at the same time.
3. Charlie's is added to a block first.
4. When Mike's transaction is then picked to go into a block, the UTxOs no longer exist. The transaction fails without needing to run the swap contracts.

Since Mike's transaction will fail without needing to run the swap script, Mike's collateral is safe. Further, the more available swaps there are, the less likely these "collisions" will occur.

#### If Cardano-Swaps reaches mass adoption, won't TVL on Cardano go down?

Yes. Yes it will. TVL is a silly metric. It is a measure of who can be most inefficient with DeFi capital.


## Conclusion
The cardano-swaps protocol has all of the desired properties of a highly scalable DEX. Thanks to the use of beacon tokens, decentralization is no longer limited by the design of DEXs. Instead, the limiting factor is now the off-chain querying. However, innovations in this space are still in the early days. The Koios API is an example of a more decentralized off-chain platform. As the technology improves, the decentralization of this protocol will improve as well.
