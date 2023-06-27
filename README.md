# Cardano-Swaps
A [p2p-DeFi protocol](https://github.com/zhekson1/CSL-DeFi-Protocols) for swapping fungible tokens on the Cardano Settlement Layer

> **Note**
> Knowledge of basic Haskell syntax and cardano-cli usage is recommended. For a list of everything that has changed from the previous version, see the [Changelog](CHANGELOG.md).

The Getting Started instructions can be found [here](./GettingStarted.md).

---
## Table of Contents 
- [Abstract](#abstract)
- [Motivation](#motivation)
- [Preliminary Discussion](#preliminary-discussion)
- [Specification](#specification)
- [Features Discussion](#features-discussion)
- [Future Directions](#future-directions)
- [FAQ](#faq)
- [Conclusion](#conclusion)

## Abstract
Cardano-swaps is a p2p-DeFi protocol for swapping fungible tokens on the Cardano Settlement Layer (CSL). It solves many of the pitfalls of current DEX implementations by empowering users to deploy their own (and interact with each others') script addresses. This leads to the formation of an order-book style "distributed" DEX, where liquidity arises from the aggregate of (composable) p2p swaps. 

## Motivation
Many DEXes on Cardano are currently implemented in ways that lock users' assets into a tightly fixed, and/or centrally maintained, set(s) of script addresses. Such design patterns are reminiscent of the EVM's accounts-based programming paradigm, and inherit many of the same downsides; scalability bottlenecks and asset/stake centralization. DEXes that hope to achieve massive scale on the CSL must adhere to a radically different approach that takes full advantage of the composability and availability guarantees offered by the eUTxO model.

## Preliminary Discussion
To appreciate the necessity of new DEX standards, it first important to understand the deficiencies of the current status-quo:

### Current DEX Deficiencies  
One consequence of centralized script addresses is the necessity for liquidity pools and LP providers as discrete, permissioned entities. LPs are a common feature of many DEXes and have a number of downsides, namely slippage/impermanent-loss and lack of delegation control. Additionally,  current implementations of order-book style DEXes (which don't use LPs) suffer from the availability challenges of permissioned batchers. No matter how performant a system of batchers is, their resources do **not** scale in proportion to the number of users unless new batchers can join permissionlessly when demand is high. Lastly, permissioned protocols lack composability with other dApps; a critical feature for scaling eUTxO-based DeFi.

A non-exhaustive list of undesirable properties (and corresponding "workaround" solutions) is outlined below. Even if some workarounds can be somewhat effective, the underlying design *principles* are suboptimal.

| Undesirable Property | Workaround "Solution" | Issues |
| :--: | :--: | :--: |
| Impermanent Loss | Yield Farming & Concentrated Liquidity | Medium - Long term unsustainability |
| Incomplete or No Delegation Control | <ul><li>Asset pool fractionalization</li><li>Indirect Delegation via Governance Tokens</li></ul> | <ul><li>Unfair distribution of Governance tokens </li><li>Unavoidable centralization of stake (Major issue for Ouroboros)</li></ul>
| Scaling/Availability Bottlenecks | Batchers, Execution/Routing Engines, and/or other middlemen | Middlemen can take advantage of their position between users and the protocol. Even if MEV is mitigated, more users --> more execution demand --> possible centralization of middlemen if the batchers are not permissionless |


### Programmable Swaps
First proposed by Axo in their original [whitepaper](https://www.axo.trade/whitepaper.pdf), programmable swaps are a more promising design choice than liquidity pools. Swaps are simply UTxOs that may be consumed if and only if the resulting TX outputs satisfy the input script's logic. Swaps can be fragmented across many user-controlled addresses, so delegation control is maintained. Users then execute swaps from each other's addresses. Since each swap is atomic and explicitly defined, in aggregate they are the optimal expression of (intra)market sentiment. This design pattern scales naturally, since there must be at *least* as many swap addresses as there are users. 

### The Cardano-Swaps Protocol
Cardano-Swaps takes inspiration from Axo's programmable swaps design, adds delegation control as a foundational feature, and, through the use of Beacon Tokens, removes the need for specialized indexers. The only remaining bottleneck is the querying capacity of existing off-chain APIs, such as Blockfrost or Koios. (This is not a limitation for users with powerful enough hardware, as they can run their own API database).

The Cardano-Swaps Protocol is broadly comprised of two steps:

1. **Prepare Swap Address** - Alice prepares a swap address for swapping Token "ABC" for Token "XYZ" by minting an ABC:XYZ Beacon Token in a "Beacon UTxO" that designates her address specifically as an ABC-for-XYZ swap address. She then (in the same or separate transaction) outputs one or more "Swap UTxO(s)" with "ABC" Tokens, with an attached `Price` Datum that designates her desired price (in "XYZ") for that particular UTxO.
   
2. **Execute Swap(s)** - Bob queries a list of ABC-for-XYZ swap addresses (via the ABC:XYZ Beacon Token), and finds some attractively priced UTxOs in Alice's swap address. He submits a transaction that consumes one (or multiple) of Alice's "Swap UTxO(s)", and one or more of his own UTxO(s) containing Alice's requested "XYZ" Token. Bob can then output Alice's "ABC" Tokens to himself in proportion to the amount of "XYZ" tokens he outputs back to Alice. Bob does not have to buy all of the contents of one of Alice's UTxOs - he buy however much he wants, and output the remaining "ABC" back to Alice in the same UTxO as her "XYZ" payment. This UTxO (containing Alice's "XYZ" tokens) remains swappable as long as some "ABC" tokens remain in it; nobody except Alice can claim the "XYZ" tokens.

These features are further explained in the [Discussion section](<#Discussion & FAQ>) below

**This Aiken version of Cardano-Swaps can handle up to 14 swaps in a single transaction with a fee of only 1.5 ADA. A transaction with a single swap only costs 0.25 ADA. This makes Cardano-Swaps up to an order of magnitude cheaper than existing Cardano DEXs.** See [benchmarks](Benchmarks.md) for details.

## Specification
The protocol is composed of one validator script per swap pair, which is used to derive the minting script for the same pair. 

### Components
An outline of the addresses, tokens, datums, and redeemers used in Cardano-Swaps.

#### Swap Address
Every swap-pair gets its own swap-address that is guarded by a pair-specific validator/spending script. This is accomplished with the help of the `SwapConfig` parameter:

``` Haskell
data SwapConfig = SwapConfig
  { swapOffer :: (CurrencySymbol,TokenName)
  , swapAsk :: (CurrencySymbol,TokenName)
  }
```

**Each combination of `swapOffer` and `swapAsk` results in a different spending script. All spending scripts are identical except for this extra parameter. The hash of the resulting spending script is used as an extra parameter to the beacon minting policy. *All beacon minting policies are identical except for this extra parameter.* As a result, every `SwapConfig` will also have its own unique beacon policy.**

#### Minting Redeemers
Two minting redeemers are introduced here, their usage is explained further below.

```Haskell
data BeaconRedeemer
  = MintBeacon -- ^ Mint pair-specific Beacon
  | BurnBeacon -- ^ Burn pair-specific Beacon
  deriving (Generic,Show)
```


#### Validator Redeemers
Three validator redeemers are introduced here, their usage is explained further below.
```Haskell
data SwapRedeemer
  = Close -- ^ recliam "Swap" or "Beacon" UTxOs
  | Update -- ^ update the asking price in "Swap" UTxOs
  | Swap -- ^ Execute a Swap
  deriving (Generic,Show)
```
Only the owner (signified by the address' staking credential) can use the `Close` or `Update` redeemers. Anyone can use the `Swap` redeemer.

#### Beacon Tokens
Cardano-Swaps uses one Beacon Token to enable efficient querying of asset-pairs, allowing users to easily find each other's swap addresses. Since the Beacon policyID itself carries all the information needed to determine which trading pair is being used, every Beacon has an empty token name - this is enforced by the minting policy.

Below is an example response from querying the beacon tokens:
``` JSON
[
  {
    "assets": [
      {
        "asset": "lovelace",
        "quantity": 15000000
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 5
      }
    ],
    "datum": {
      "price": {
        "denominator": 600000,
        "numerator": 1
      }
    },
    "swap_address": "addr_test1zqquvxk3d44kkry0a502f60vq6qkr8kq0f82h7vpttvrvmpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqhzu5re",
    "utxo_id": "14779637d6b6d8631f2af0952c2e31a7f800ae4b12d1ca95e843bdbd0ffb4d7d#0"
  }
]
```

Only one UTxO was found, containing Lovelace (ADA) and 5 units of a native token. The asking price is one unit of the desired native token for 0.6 ADA. This response has everything a user needs to swap with the address.

##### Burning Requirements
Since minting and spending beacons are so heavily controlled, there is no reason to regulate burning. Burning is always allowed as long as the burn redeemer is only used to burn beacons.

#### Datums
Two different inline datums are used to enforce minting and validator logic:

``` Haskell
-- | Swap Datum
type Price = Rational -- ^ askedAsset/offeredAsset
data SwapDatum
  = BeaconSymbol CurrencySymbol -- ^ Datum stored with beacons.
  | SwapPrice Price -- ^ Datum stored with swappable UTxOs.
```

> **Note** 
> This datum is different than the previous version of Cardano-Swaps, which used a product type.

##### BeaconSymbol Datum
The `BeaconSymbol` datum prevents misuse of beacons. The validator forces all assets with the supplied policyID to be burned instead of being withdrawn. This ensures that beacons can only ever be located at swap-addresses. **If the wrong policy id is supplied, assets can be locked forever.** Only the "Beacon" UTxO containing the beacon needs to use the `BeaconSymbol` datum, whereas "Swap" UTxOs use the `SwapPrice` datum. Cardano-Swaps CLI handles this part of the datum automatically, preventing accidental misuse.

##### SwapPrice Datum
The `Rational` type is a fraction (decimal types do not work on-chain). All prices in Cardano-Swaps are relative (similar to limit orders in an order-book exchange). Swaps are always priced in askedAsset/offeredAsset. For example, if $ADA is being offered for $DUST at a price of 1.5 (converted to 3/2), the contract requires that 3 $DUST are deposited for every 2 $ADA removed from the swap address. Ratios of DUST:ADA >= 3/2 will pass, while ratios < 3/2 will pass. 

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets at the swap address or UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same Tx. Since every user explicitly defines their desired swap ratios, oracles are not required. The "global" price naturally emerges where the local bids and asks meet - just like an order-book.

> **Note**
> 
> In previous versions, users could have specified a price in units of ADA and the script would convert it to units of Lovelace during execution. This feature was removed to save on execution costs. Now, all prices for ADA must be in units of Lovelace.


### Cardano-Swaps Lifecycle
Below is an in-depth look at Cardano-Swaps logic.

#### Preparing a Swap Address
Swap "writers" must first prepare a swap address by using the `MintBeacon` Redeemer in the following manner:

![Prepare-Swap](./images/Prepare.jpg)

In order to mint beacons with this redeemer, **all of the following must be true**:

1. Only one Beacon is minted per Tx.
2. The minted Beacon uses an empty token name.
3. The Beacon is minted to an address whose payment credential is of the corresponding validator for that particular trading pair.
4. The Beacon is minted to an address with a staking credential (either a pubkey or a script).
5. The datum of the output containing the beacon must have the proper `BeaconSymbol` datum with the policyID of the minting policy.
6. The beacon must be stored with a minimum of 20 ADA.

Once the beacon is minted to the swap address, the spending script does not allow consuming the "Beacon" UTxO *unless* the beacon is being burned. This prevents beacons from being sent to unrelated addresses.

> **Note**
> 20 ADA is used as a hardcoded deposit for "Beacon UTxOs" to prevent malicious users from spamming other users' querying capacities for a particular trading pair. This deposit can be reclaimed when closing the address.


##### Closing a Swap Address
Open "Swap" UTxOs can be closed/reclaimed by the address owner (signified by the address' staking credential)

The `Close` redeemer allows the owner  to recover the deposit stored with the reference script, and make the address undiscoverable by burning the beacon. **In order to reclaim the deposit, the beacon must be burned.** The requirements for successfully using the `Close` redeemer are:

1. All beacons among Tx inputs must be burned.
2. The staking credential must signal approval (via key or script)

> **Note**
> 
> In the previous version, the `Close` redeemer also checked the transaction outputs to make sure any new outputs to the script were valid. This feature was not necessary and was removed.


#### Updating Prices
The `Update` redeemer allows users to update their asking prices by modifying the `swapPrice` datum field in the "Swap" UTxO(s):

![Update Swap](./images/Update.jpg)

The `Update` redeemer includes checks to ensure the new datum is properly used:

1. No Beacons among Tx inputs.
2. The staking credential must signal approval (via key or script)
3. All new outputs to the address must contain the proper datum:
    - The new `SwapPrice` datum must be > 0.


#### Executing a Swap
Any Cardano user can execute an available swap using the `Swap` Redeemer:

![Simple Swap](./images/Simple-Swap.jpg)

The `Swap` redeemer checks all of the assets leaving the swap address and all of the assets entering the swap address. For a swap to be executed, all of the following must be true:

1. No inputs have a `BeaconSymbol` datum.
2. All "Swap" inputs have a `swapPrice` > 0.
3. All outputs to the swap address contain the proper datum:
    - inline `SwapPrice` datum with the price == weighted average price of all swap inputs
4. `QuantityOfferedAssetTaken` * weighted average price <= `quantityAskedAssetGiven`
5. Only the offered asset (as defined in `SwapConfig`) is leaving the swap address.

Custom error messages are included to help troubleshoot why a swap failed. The weighted average price must match exactly what the swap contract calculates. To help with this, `cardano-swaps` automatically calculates the weighted price for all inputs.


## Features Discussion
A discussion of features unique to Cardano-Swaps, in addition to the features shared by all p2p-DeFi protocols.

### Composable Swaps
Since multiple swaps are combinable into a single transaction, any arbitrarily complex swap transaction can be created. The only limits are the size and execution budgets of transactions, which are Cardano node parameters.

![Three-Swap](./images/Three-Swap.jpg)


### Emergent Liquidity
Liquidity in cardano-swaps is an *emergent* property; it arises from the (healthy) incentive for arbitragers to compose complex swaps. As long as the entry and exit swap-pairs have enough liquidity, arbitragers can spread liquidity into less liquid swap pairs. As a bonus, **the very nature of *illiquidity* implies great arbitrage opportunities**. The more illiquid a swap pair, the greater the potential arbitrage profits. Participating in arbitrage/market-making is permissionless, so anyone can create their own algorithms to find the most profitable "path" through the sea of available swaps.


### Benchmarks and Fee Estimations (YMMV)
Thanks to the efficiency of Aiken, Cardano-Swaps is capable of composing up to 14 different swaps in a single transaction, with a total transaction fee of 1.5 ADA. A transaction with a single swap only costs 0.25 ADA. These results likely make this the cheapest "DEX" on Cardano by an order of magnitude.

Given the performance of these Aiken contracts, Cardano-Swaps is more than performant enough for the current state of Cardano. **No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found in the [Benchmarks](Benchmarks.md) file.


## Future Directions
Features and considerations for future versions of Cardano-Swaps:

### Offer-Based Swap Addresses
Currently, swap addresses are unique for every trading pair. This necessitates many distinct Beacon UTxOs (and associated 20 ADA deposits), even if users are offering the same asset in exchange for varying assets. Future versions of Cardano-Swaps may instead utilize "Offer-based" swap addresses, which are unique only to the asset being offered. The desired "exchange" asset would be specified in the datum of every "Swap" UTxO at the address. This would dramatically lower Beacon UTxO deposit fees, as only one such deposit is needed per "Offer" asset, per user. It would also make querying more expressive, allowing users to filter through available swaps not only by pairs, but by Offers too.

> **Note**
> This feature will introduce a new Beacon Token for querying and control flow.


### Redundant Script Executions
Currently, plutus validator scripts must be executed once for *every* UTxO being consumed from a script address (this is the case for Plutus as a whole, not just Cardano-Swaps). If multiple "Swap" UTxOs from the same address are consumed as inputs, the validator must execute for each of them, even though their spending logic is exactly the same. These executions are "redundant", in a sense, resulting in a (low severity) composability bottleneck. However, this protocol is currently more than performant enough for expected demand. If at some point demand exceeds expectations, fixing the redundant executions could give a slight boost to composability. As of now, such changes are simply unnecessary.

> **Note**
> 
> [Present and future discussions with the community](https://github.com/cardano-foundation/CIPs/pull/418) will help determine if this feature is indeed warranted.

### Swap Collisions
Recall the composition example above. What would happen if another user simultaneously submits a transaction using *one* of the same "Swap" UTxOs as an input? Whichever transaction is processed by (most) nodes first will succeed, and the other will fail entirely. Arbitragers/market-makers will be in constant competition with one another to query and execute the most profitable swaps at any point in time, and must strike a balance between simple swaps and complex composed swaps. There is a lot of stochasticity here; the higher the ratio of open swaps to arbitragers is, the lower the overall chances of a collision, but the more attractive a particular swap is, the higher the chances of *that* particular collision. And "attractiveness" depends not only on the price of one swap, but how that price relates to all possible compositions involving that swap.


### Mainnet Readiness
Currently, Aiken is still in the alpha phase and, by extension, so is this protocol. Furthermore, there are some features that should be tried to determine their trade-offs. Mainnet release ideally should happen after:

1. Aiken stabilizes
2. Cardano-Swaps features stabilize
3. Cardano-Swaps undergoes a security audit

That being said, this protocol is fully peer-to-peer. If some community members are willing to accept the risks and are comfortable with the current features, nobody can stop them from using this protocol right now. 


## Conclusion
The Cardano-Swaps protocol has all of the desired properties of a highly composable p2p-DEX. Thanks to the use of Beacon Tokens, decentralization is no longer limited by the design of DEXs. Instead, the limiting factor is now the off-chain querying. However, innovations in this space are still in the early days. The Koios API is an example of a more decentralized off-chain platform. IOG's Marconi project may lead to significant reductions in hardware requirements for local querying. As the technology improves, so too will the decentralization of the protocol.