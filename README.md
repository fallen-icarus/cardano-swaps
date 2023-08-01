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
- [UTxO Concurrency Discussion](#utxo-concurrency-discussion)
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

## Specification
The protocol is composed of one universal validator script for all swap pairs, and one minting script for each offer asset. For example, all trading pairs where ADA is being offered (eg, DJED/ADA, AGIX/ADA) will share a minting policy while all trading pairs where DJED is being offered will get a different minting policy. The asset name for all beacons is:

```Bash
sha2_256( ask_policy_id ++ ask_asset_name )
```

Therefore, the beacon policy id represents the offer asset and the beacon asset name represents the ask asset. This allows for more expressive queries since it makes it possible to query all swaps where XYZ is being offered, regardless of the asset being asked for, in addition to being able to query all swaps for a specific trading pair.

### Components
An outline of the addresses, extra parameters, tokens, datums, and redeemers used in Cardano-Swaps.

#### Swap Address
All users get a single swap address where all of their swaps will be located. This universal validator can enforce all possible swaps for the user. And since users only need to worry about a single address, it dramatically simplifies the user experience.

#### AssetConfig
In order to create a unique beacon minting policy for each offer asset, the `AssetConfig` extra parameter is used in addition to the validator hash of the spending policy. Here is the definition for the `AssetConfig`:

```Haskell
data AssetConfig = AssetConfig
  { assetId :: CurrencySymbol -- ^ Policy ID
  , assetName :: TokenName -- ^ Asset Name
  }
```

**Every combination of Policy ID and Asset Name in `AssetConfig` will result in a different beacon minting policy.**

#### Minting Redeemers
Two minting redeemers are introduced here, their usage is explained further below.

```Haskell
data BeaconRedeemer
  = MintBeacons [AssetConfig] -- ^ A list of assets being asked for.
  | BurnBeacons
```

#### Validator Redeemers
Three validator redeemers are introduced here, their usage is explained further below.
```Haskell
data SwapRedeemer
  = CloseOrUpdate
  | Swap
```

Only the owner (signified by the address' staking credential) can use the `CloseOrUpdate` redeemer. Anyone can use the `Swap` redeemer.

#### Beacon Tokens
Cardano-Swaps uses one Beacon Token to enable efficient querying of asset-pairs, allowing users to easily find each other's swap addresses.

Below is an example response from querying the beacon tokens:
``` JSON
[
  {
    "assets": [
      {
        "asset": "lovelace",
        "quantity": "10000000"
      },
      {
        "asset": "04d0873d1fb3316c65f8a40dbd4571b48118e721d8993a6847d51f80.6f245d4be7333223d88e7211c4de116208b8a3898c9a196e0579391fd03c8860",
        "quantity": "2"
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": "150"
      }
    ],
    "datum": {
      "ask_id": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d",
      "ask_name": "4f74686572546f6b656e0a",
      "beacon_id": "04d0873d1fb3316c65f8a40dbd4571b48118e721d8993a6847d51f80",
      "beacon_name": "6f245d4be7333223d88e7211c4de116208b8a3898c9a196e0579391fd03c8860",
      "offer_id": "",
      "offer_name": "",
      "price": {
        "denominator": 200000,
        "numerator": 3
      }
    },
    "swap_address": "addr_test1zqvkss4kwg5y0h5qxxw7fe7rf02fmcxs0x2knew3yzvzad3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqykhv32",
    "utxo_id": "6db024cde5401d4a8e58e10170f543da29a8534208277b83355dd538519ea550#0"
  }
]
```

This response has everything a user/owner needs to interact with the UTxO.

#### Datums
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
  }
```

The beacon information in the datum prevents misuse of beacons. The validator forces all beacons to be burned instead of being withdrawn. This ensures that beacons can only ever be located at swap-addresses. **If the wrong beacon information is supplied, assets can be locked forever.** For this reason, all minting policies check that the datums have the proper information; the mint transaction will fail if they do not.

The `Rational` type is a fraction (decimal types do not work on-chain). All prices in Cardano-Swaps are relative (similar to limit orders in an order-book exchange). Swaps are always priced in askedAsset/offeredAsset. For example, if $ADA is being offered for $DUST at a price of 1.5 (converted to 3/2), the contract requires that 3 $DUST are deposited for every 2 $ADA removed from the swap address. Ratios of DUST:ADA >= 3/2 will pass, while ratios < 3/2 will pass. 

When engaging in swaps, it is only necessary that the desired swap ratio is met; **not all assets at the swap address or UTxO must be swapped.** For example, if there is 100 ADA in a swap address requesting 2:1 for DUST, a user may swap 20 ADA, as long as they return 80 ADA and 10 DUST in the same Tx. Since every user explicitly defines their desired swap ratios, oracles are not required. The "global" price naturally emerges where the local bids and asks meet - just like an order-book. 

**All prices for ADA must be in units of Lovelace.**


### Cardano-Swaps Lifecycle
Below is an in-depth look at Cardano-Swaps logic.

#### Creating a Swap
Swap "writers" must first create a swap by using the `MintBeacons` Redeemer in the following manner:

In order to mint beacons with this redeemer, **all of the following must be true**:

1. Only the beacons for the target ask assets can/must be minted - they must appear in the redeemer.
2. The token name for the beacons must be: sha2_256( ask_policy_id ++ ask_asset_name )
3. The beacons are minted to an address whose payment credential is of the universal spending validator.
4. The beacons are minted to an address with a staking credential (either a pubkey or a script).
5. The beacons must be stored individually (i.e., 1 per UTxO) at the address.
6. The datum of the output containing the beacon must have the proper inline datum:
      - beaconId == this policy id
      - beaconName == sha2_256( ask_policy_id ++ ask_asset_name )
      - offerId == assetId of the extra parameter passed to the minting policy.
      - offerName == assetName of the extra parameter passed to the minting policy.
      - askId == assetId for the corresponding `AssetConfig` in the redeemer.
      - askName == assetName for the corresponding `AssetConfig` in the redeemer.
      - price denominator > 0.
      - price > 0.
7. The beacons must be stored with some of the offer asset.
8. No extraneous assets can be stored in the swap UTxO.

Once the beacon is minted to the swap address, the spending script does not allow closing the swap *unless* the beacon is being burned. This prevents beacons from being sent to unrelated addresses.

All swaps must now be stored with the proper beacon. Due to this requirement, the absolute minimum possible value for a swap UTxO to have is 1.823130 ADA - this is if the offer asset is ADA. This is instead of the 20 ADA deposit that was required in the previous versions. This minimum value can be reclaimed upon closing the swap so it should be thought of as a deposit. **All open swaps require a deposit of at least 1.823130 ADA.** While this behavior is due to the current protocol parameters, it is actually a good thing since it helps prevents denial-of-service attacks from having a lot of "zombie swaps" open (i.e., UTxOs where there is not enough of the offer asset for other users to swap).

It is possible to open swaps for multiple different trading pairs in a single transaction. The minting policies are capable of still ensuring that all beacons are stored properly (with the proper value and datum). For example, since each beacon's datum is specific to that beacon, mixing up beacons and datums will cause the minting transaction to fail.

This redeemer allows burning any beacons to enable composition with the `CloseOrUpdate` redeemer.


##### Closing or Updating a Swap
Open Swap UTxOs can be closed or updated by the address owner (signified by the address' staking credential).

The `CloseOrUpdate` redeemer allows the owner to recover the deposit stored with the swap. **In order to reclaim the deposit, the beacon(s) must be burned.** As the redeemer name suggests, swaps can also be updated instead. The requirements for successfully using the `CloseOrUpdate` redeemer are:

1. The staking credential must signal approval (via key or script)
2. Any beacons not burned must be re-output to this address with the proper inline SwapDatum.
    - all fields the same as the input datum except for the updated price.
    - the price denominator must be > 0.
    - the price must be > 0.
3. The beacons must be stored with some of the offer asset.
4. No extraneos assets can be stored in the swap UTxO.
5. The beacosn must be stored individually.

Requirement 2 guarantees that beacons from other trading pairs cannot be combined into one output UTxO; all trading pairs must get their own swap UTxOs.

Requirement 5 forces the user to burn extra beacons. Extra beacons can arise when swap inputs are consolidated into one output during an asset swap.

This redeemer can be used with either beacon redeemer since both allow burning. If beacons only need to be burned, it is cheaper to use the `BurnBeacons` redeemer. However, if even a single beacon must be minted, the `MintBeacons` redeemer must be used. This behavior enables the swap owner to close one swap and open another one for a different trading pair in the same transaction.

#### Executing a Swap
Any Cardano user can execute an available swap using the `Swap` Redeemer.

At a high level, the `Swap` redeemer checks all inputs from Alice's address with beacon XYZ and all outputs to Alice's address with beacon XYZ. The asset flux for this specific swap only depends on these inputs and outputs. Any other inputs and outputs are ignored in this specific execution. This logic works because a script is executed once for every UTxO spent from the address. If input 1 is for beacon XYZ and input 2 is for beacon ABC, the first execution can be dedicated to beacon XYZ and the second execution can be dedicated to ABC. The net transaction will only succeed if all executions succeed. In short, each execution of the smart contract is dedicated to a single trading pair. This behavior allows composing swaps of different trading pairs that are located at the same address.

For a swap execution to be successfull, all of the following must be true:

1. The input must contain the beacon for that trading pair - the required beacon is gotten from the datum for that UTxO.
2. All beacons re-output to this address must have the proper inline SwapDatum.
    - all fields the same as the input datum except the price.
    - the price must be the weighted avg of all relevant input prices.
3. Offered asset taken * weighted avg price <= asked asset given.
4. Only the offered asset can leave and only the ask asset can be deposited. ADA can always be deposited in case the minUTxOValue increased.

Requirement 1 guarantees that all invalid UTxOs (those missing beacons) belong to the address owner and that swap inputs have a valid price: denominator > 0 and price > 0.

Requirement 2 guarantees that beacons from other trading pairs cannot be combined into one output UTxO. If they could be, it would skew the asset flux calculation and possibly cause double satisfaction of swaps.

Requirement 4 requires that all beacons from the address must be returned to the address. Beacons of the same trading pair are allowed to be consolidated into a single output during an asset swap. The reason why only the ask asset can be deposited is to help minimize any work needed to be done by api endpoints when the beacons are queried - it is better to make UTxOs as small as possible.

Custom error messages are included to help troubleshoot why a swap failed. The weighted average price must match exactly what the swap contract calculates. To help with this, `cardano-swaps` can automatically calculates the weighted price for all inputs.

## Features Discussion
A discussion of features unique to Cardano-Swaps, in addition to the features shared by all p2p-DeFi protocols.

### Composable Swaps
Since multiple swaps are combinable into a single transaction, any arbitrarily complex swap transaction can be created. The only limits are the size and execution budgets of transactions, which are Cardano node parameters.


### Emergent Liquidity
Liquidity in cardano-swaps is an *emergent* property; it arises from the (healthy) incentive for arbitragers to compose complex swaps. As long as the entry and exit swap-pairs have enough liquidity, arbitragers can spread liquidity into less liquid swap pairs. As a bonus, **the very nature of *illiquidity* implies great arbitrage opportunities**. The more illiquid a swap pair, the greater the potential arbitrage profits. Participating in arbitrage/market-making is permissionless, so anyone can create their own algorithms to find the most profitable "path" through the sea of available swaps.

### Offer Based Queries
Offer based queries allow arbitragers to find profitable swap compositions based on current market demand. The previous version required the arbitrager to know what trading pairs to check in advance. But what if there was a token that they had never heard of? This opportunity would be missed. With offer based queries, these unheard of tokens would be returned by the first query and the arbitrager's algorithm can decide which other assets to query based on those results.

In short, the previous version effectively had hard-coded composition paths while the new version allows for natural discovery of composition paths.

### Single Swap Address
This dramatically improves the usability of the DEX since users or frontends don't need to manage a possibly infinite set of addresses.


### Benchmarks and Fee Estimations (YMMV)
Due to more information being in each swap datum, the performance of this version is slightly less than the previous version. This version is capable of composing up to 9 different swaps in a single transaction (compared to 14 in the previous version).

**No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found in the [Benchmarks](Benchmarks.md) file.


## Future Directions
Features and considerations for future versions of Cardano-Swaps:

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

## UTxO Concurrency Discussion
There is a belief that liquidity pools are required for DEXs due to the concurrency bottlenecks created by having users compete for swap UTxOs. There are a few issues with this belief but before getting to that, what do people mean by "concurrency bottleneck"? Doesn't cardano-swaps scale with the number of users? The answer is yes but only on the sell side. There are only as many swap UTxOs as there are sellers. In bull markets, where the number of buyers outnumber the number of sellers, the buyers will be racing to get the swaps first.

Is this a problem that cardano-swaps should try to fix? Are liquidity pools actually better since buyers can be bundled together through batchers? The answer is more nuanced than what these questions suggest. Consider a TradFi exchange - they are all order books. And yet, order books are just an aggregate of limit orders that are first come, first serve. Therefore, this is the exact same condition that occurs with cardano-swaps. Despite this, TradFi is able to handle this concurrency issue without liquidity pools.

How do they do this? The answer is to have other businesses take on that risk. These businesses are paid to absorb the risk from the end user. Consider a market maker, they fill the gaps in the order book to provide immediate liquity with the expectation that they will be able to reverse the trade for a profit later. You can think of it as *you buy from us now and we will settle against the order book later*. These market makers are essentially batchers that try to turn a profit through taking on the end-user's risk.

Whenever there is risk, there will always be a demand for a business to minimize the risk (e.g. hedges, insurance, etc.). These businesses are not baked into the market, they are add-ons. Liquidity pools try to combine the functionality of the DEX with the risk management of the add-on businesses. In the process, it winds up being bad at both. The cardano-swaps DEX takes the approach *just do one thing and do it well, and allow businesses to easily be built on top of it*. Businesses can undoubtedly be created to act as an intermediary between end users and the cardano-swaps order book so that users don't need to worry about concurrency issues.

"Hold on... I thought blockchain is supposed to *dis-intermediate*." This is a common belief but it is not accurate. The more correct belief is: *blockchain is supposed to eliminate the **requirement** to use intermediaries*. Consider banking, is it possible to live in the developed world without a bank account? How would you order things online? How would you get a loan? The world is currently set up so that prospering in the developed world **requires** a bank account. The banks know this and therefore don't need to treat users as true *customers*. What if you didn't need to use a bank? You could if you wanted to but you don't have to. Banks would behave very differently since they know users don't actually need them. The world would shift from *banking out of necessity* to *banking only if it adds value*. Liquid democracy is another example, you can delegate your vote if you want to but you can always vote directly. The very existence of the *do it yourself* option acts as a check on the behavior of intermidiaries.

This protocol is designed with this very philosophy in mind - Alice can choose to interact with it through an intermediary (and therefore minimize her concurrency risk) or she can choose to interact with it herself. So is it an issue that the sell side demand may not always be able to keep up with the buy side? Probably not. 

## Conclusion
The Cardano-Swaps protocol has all of the desired properties of a highly composable p2p-DEX. Thanks to the use of Beacon Tokens, decentralization is no longer limited by the design of DEXs. Instead, the limiting factor is now the off-chain querying. However, innovations in this space are still in the early days. The Koios API is an example of a more decentralized off-chain platform. As the technology improves, so too will the decentralization of the protocol.