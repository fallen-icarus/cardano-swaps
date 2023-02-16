# Cardano-Swaps

:warning: Knowledge of basic Haskell syntax and cardano-cli usage is assumed.

The Getting Started instructions can be found [here](GettingStarted.md).

---
## Table of Contents
- [Motivation](#motivation)
  - [Liquidity Pools](#liquidity-pools)
  - [Programmable Swaps](#programmable-swaps)
  - [Cardano-Swap DEX](#the-cardano-swaps-dex)
- [Personal Contracts](#personal-contracts)
  - [How Are Addresses Protected](#if-all-users-share-a-spending-script-how-are-their-addresses-protected)
- [Beacon Tokens](#beacon-tokens)
  - [Beacon Tokens with Cardano-Swaps](#beacon-tokens-with-cardano-swaps)
  - [Generalizing Beacon Tokens](#generalizing-beacon-tokens)
- [The DEX's Inline Datum](#the-dexs-inline-datum)
  - [`swapPrice`](#swapprice)
  - [`swapBeacon`](#swapbeacon)
- [Swap Contract Logic](#the-swap-contract-logic)
  - [Close Redeemer](#close-redeemer)
  - [Update Redeemer](#update-redeemer)
  - [Swap Redeemer](#swap-redeemer)
- [Composable Atomic Swaps](#composable-atomic-swaps)
- [Liquidity](#liquidity)
  - [The Contrived Example](#the-contrived-example)
  - [The Realistic Example](#the-realistic-example)
  - [Liquidity Naturally Flows To The Less Liquid Pairs](#liquidity-naturally-flows-to-the-less-liquid-pairs)
  - [What If Two Arbitragers Compete For The Same Swap?](#what-if-two-arbitragers-compete-for-the-same-swap)
- [Benchmarks and Fee Estimations](#benchmarks-and-fee-estimations-ymmv)
- [Upgradability](#upgradability)
- [Frontend Agnostic](#frontend-agnostic)
- [Conclusion](#conclusion)
- [FAQ](#faq)
  - [Why Are Reference Scripts Required?](#if-all-users-have-direct-access-to-the-spending-script-for-that-trading-pair-why-are-reference-scripts-required)

---
## Motivation
Any DEX that operates on a Proof-of-Stake (PoS) blockchain **MUST** include delegation control as a foundational feature for the protocol. It is existentially important for the long-term sustainability of the underlying blockchain. Yet, all Cardano DEXs either cannot give users full delegation control by design or delegation control is just an afterthought.

### Liquidity Pools
The most commonly used DEX design is liquidity pools which have the downsides of:
 
  1. Impermanent Loss
  2. Loss of Delegation Control
  3. Difficulty of Concurrency

Each of these downsides have their own corresponding "solution": yield farming, governance tokens, and batchers, respectively. However, these "solutions" have downsides of their own:
 
  1. The long term sustainability of yield farming is questionable.
  2. Fairly distributing governance tokens is difficult.
  3. Batchers are effectively middle-men that can take advantage of their position between users and the protocol.

Even more concerning is that, even if governance tokens are fairly distributed, this doesn't change the fact that a liquidity pool can only be delegated to one stake pool. Fractionalizing liquidity pools helps but it still results in a more centralized PoS blockchain than if each user was able to delegate their assets independently.

### Programmable Swaps
Programmable Swaps are a more promising design than liquidity pools and were first proposed by Axo (formerly known as Maladex). However, in Axo's [whitepaper](https://www.axo.trade/whitepaper.pdf), there is no mention of giving users full delegation control of their own assets. When they were asked on Discord about it, they said it was possible but delegation control would not be included in the first version.

### The Cardano-Swaps DEX
Cardano-Swaps took inspiration from Axo's programmable swaps design but added delegation control as a foundational feature. `cardano-swaps` is the name of the CLI program included to help use the DEX.

Interestingly, by starting with the requirement of full delegation control, other nice properties naturally emerged:

  1. Composable atomic swaps.
  2. Users maintain custody of their assets at all times.
  3. Naturally concurrent and gets more concurrent the more users there are. No batchers are required.
  4. Liquidity naturally spreads to all trading pairs instead of being siloed into specific trading pairs.
  5. There is no impermanent loss.
  6. ADA is all you need to interact with the DEX.
  7. Upgradability can happen democratically.
  8. Easy to integrate into any frontend.

In addition to these nice properties, the novel use of *Beacon Tokens* can be generalized to any application on Cardano.

---
## Personal Contracts
In order for users to maintain full delegation control of their assets, it is required for user assets to be kept siloed. The reason for this is that, on Cardano, all ADA at an address is either delegated entirely or not at all (atomic delegation is not possible). So for a user to maintain delegation control of their assets, only that user's assets should be at the address he/she is using.

A cardano address is made up of both a payment credential and a staking credential. As long as the staking credential is unique to that user, that user will maintain full delegation control for that address. With this in mind, every user is given the same spending script to use for the address' payment credential and are required to use their own staking credential (either a pubkey or a staking script). To force the use of a staking credential, the DEX is designed so that it is not possible to mint a beacon token (discussed later) to an address without a staking credential.

### If all users share a spending script, how are their assets protected?
The spending script gets the staking credential from the address of the utxo being spent at run-time. When an owner related action is being performed (closing or updating positions), the spending script requires that the staking credential "signals approval" of the action:

- If the staking credential is a pubkey, then the staking pubkey must sign the transaction.
- If the staking credential is a script, then the script must be executed in the same transaction.

:note: It is possible to execute a staking script even if 0 ADA is withrawn from a rewards address. The only requirement to use staking scripts like this is that the associated stake address must be registered and delegated. **You can start using the stake address like this the moment the registration+delegation transaction gets added to the chain. You do not need to wait an epoch.**

### Why not just give each user their own spending script?
The reason is that it is much harder to use beacon tokens when the address is made up of completely unique credentials. The beacon tokens should only be usable with addresses of the DEX. If their is no way to clearly distinguish a DEX's address from any other address, guaranteeing proper usage of the beacon tokens is virtually impossible. v1.0.0 of Cardano-Swaps used unique spending scripts; you can read about the limitations in the v1.0.0 commit README.

### Delegating
With each user having their own DEX address, delegation the address is identical to delegating a traditional payment address that uses either a staking pubkey or a staking script. You can check out [delegation section](GettingStarted.md#delegate-the-swap-address) of GettingStarted.md for an example using a staking pubkey.

---
## Beacon Tokens
Upon reading how all users have their own DEX addresses, it is natural to ask how potential "swappers" can find all available swaps and the information needed to remotely swap with them. The answer is to use beacon tokens.

Using [Koios](https://api.koios.rest/#overview) or [Blockfrost](https://docs.blockfrost.io/) apis, it is possible to find all addresses that contain a specific native token. Once you have the address, you can then use Koios or Blockfrost again to get all utxos locked at those addresses.

| Task | Koios Api | Blockfrost Api |
|--|--|--|
| Addresses with a beacon | [api](https://api.koios.rest/#get-/asset_address_list) | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1addresses/get)|
| UTxOs at the address | [api](https://api.koios.rest/#post-/address_info) | [api](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos/get)|

The utxos api also returns which utxos contain reference scripts. This is how users can remotely execute swap contracts.

Technically, all native tokens can be used as beacons like this but this feature is usually not the intended one. The name *Beacon Token* refers to any native token whose only purpose is to act as a beacon.

### Beacon Tokens with Cardano-Swaps
Every trading pair gets its own DEX spending script. This is accomplished with the help of an extra parameter. Here is the data type of that extra parameter:

``` Haskell
data SwapConfig = SwapConfig
  { swapOffer :: (CurrencySymbol,TokenName)
  , swapAsk :: (CurrencySymbol,TokenName)
  }
```

Every possible combination of `swapOffer` and `swapAsk` will result in a completely different spending script. *All spending scripts are identical except for this extra parameter.* The hash of the resulting spending script is then used as an extra parameter to the beacon policy. *All beacon policies are identical except for this extra parameter.* As a result, every `SwapConfig` will also have its own unique beacon policy. 

Since the beacon policy id itself carries all the information needed to distinguish what trading pair is being used, every beacon uses the empty token name. The beacon policies force the use of the empty token name.

#### Minting Requirements
Minting beacons is tightly controlled. In order to mint beacons, all of the following must be true:

1) Only one beacon is minted in the tx.
2) The minted beacon uses the empty token name.
3) The beacon is minted to an address protected by the DEX spending script for that trading pair.
4) The beacon is minted to an address with a staking credential (either a pubkey or a script).
5) The beacon is stored ina utxo containing the reference script of the DEX's spending script for that trading pair.
6) The datum of the output containing the beacon must contain the proper beacon symbol.

Once the beacon is to the DEX's address, the spending script does not allow consuming the beacon's utxo unless the beacon is being burned. This makes sure that the beacons can never be found at an unrelated address.

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

Only one utxo was found and that utxo only has lovelace in it. Notice how this utxo's required reference script ID was also returned. This response has everything a user needs to remotely swap with the address.

### Generalizing Beacon Tokens
While these beacons are used to broadcast all necessary information for remotely executing swaps (reference script utxos and available swap utxos), they can be used for broadcasting ANY information tied to:

1. The address they are inside
2. The utxo they are stored in
3. The information for any transaction they have ever been in 

For example, the last use case allows the beacons to be used to also broadcast the metadata of all transactions they were ever part of. This feature can create a trustless "metadata history" trail. Here is the Blockfrost [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1transactions/get) that can give you the transaction history for a beacon. Here is the Koios [api](https://api.koios.rest/#get-/asset_txs) for the same thing. Once you have the transaction history you can use this Blockfrost [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1metadata/get) to get the metadata for each transaction. Here is the Koios [api](https://api.koios.rest/#post-/tx_metadata) for the same thing.

All of this information is broadcasted automatically and trustlessly by the beacon tokens; no configuration of the tokens is necessary. You can simply decide which information to use. The only requirement is that you make the beacon token unique for each kind of information.

---
## The DEX's Inline Datum
In order for other users to see the asking prices, all datums for the DEX must be inline datums. The DEX's logic enforces this behavior whenever possible. Here is the DEX's datum type:

``` Haskell
-- | Swap Datum
type Price = Rational -- ^ askedAsset/offeredAsset
data SwapDatum = SwapDatum
  {
    swapPrice :: Price,
    swapBeacon :: Maybe CurrencySymbol -- ^ Policy id for the DEX's beacon token
  }
```

### swapPrice
The `Rational` type is a fraction. Fractions are used on-chain due to the inability to properly use decimal types on-chain. The good news is there is no loss of functionality from using fractions. Users are able to supply decimals to `cardano-swaps` and it will properly convert the decimal to `Rational`.

All prices on Cardano-Swaps are local. You can think of them as limit orders on an order book exchange. The price is always askedAsset/offeredAsset. So if you are offering ADA for DUST at a price of 1.5 (converted to 3/2), then the DEX enforces that 3 DUST are deposited for every 2 ADA removed from the swap address. So 15 DUST for 10 ADA is valid but 10 DUST for 10 ADA is not and will fail. 

By using prices like this, no oracles are needed for this DEX. Every user can set their own desired swap ratio based off what they believe the assets to be worth. The "global" price naturally emerges where the local bids and asks meet.

A zero or negative price means the assets are effectively free. A malicious user may deposit a utxo with a negative price in the datum in order to steal user funds. To prevent this, swaps will fail unless all prices are greater than 0.

When ADA is part of the pair, the price **MUST** be in units of ADA. The swap logic will correctly convert to lovelace when necessary. This was to improve usability. If you accidentally provide the price in terms of lovelace, the price will still undergo the conversion when the script executes and produce undesired behavior.

### swapBeacon
The `swapBeacon` field for the datum is necessary for the DEX to prevent misuse of the beacons. The DEX forces all assets with the supplied policy id to be burned instead of being withdrawn. This ensures the beacons can never be found in an address unrelated to the DEX. **If you supply the wrong policy id, your assets can be locked forever since you will not be allowed to withdraw them if they have the supplied policy id.** Only the utxo containing the beacon needs to use `Just beaconSym`; all active swaps can use `Nothing` for this field. `cardano-swaps` cli handles this part of the datum for you so you actually have to go out of your way to accidently lock funds by using the wrong `swapBeacon`. Just use `cardano-swaps` for creating datums and you will always have the correct datum.

---
## The Swap Contract Logic
DEX contracts have three possible actions, aka redeemers:

1. `Close` - withdraw any utxo located at the swap address and burn the beacon
2. `Update` - update the asking price for the utxos at the swap address
3. `Swap` - try swapping with assets at the swap address

Only the owner (signified by the address' staking credential) is allowed to use the `Close` or `Update` redeemers. Anyone can use the `Swap` redeemer.

### `Close` Redeemer
The `Close` redeemer makes it possible for the owner (signified by the address' staking credential) to recover the deposit stored with the reference script and make the address undiscoverable by burning the beacon. In order to reclaim the deposit, the beacon must be burned. The requirements for successfully using the `Close` redeemer are:

1. All beacons among tx inputs must be burned.
2. The staking credential must signal approval:
    - pubkey must sign
    - script must be executed in the same tx
3. Any new outputs to the address must contain the proper datum:
    - `swapPrice` > 0
    - `swapBeacon` == Nothing

### `Update` Redeemer
As previously mentioned, Cardano-Swaps uses inline datums. However, sometimes the owner will want to change the asking price of his/her positions. This redeemer allows the owner to change the inline datum attached to his/her utxos. This action includes checks to ensure the new datum is properly used. The requirements for a successful update are:

1. No beacons among tx inputs.
2. The staking credential must signal approval:
    - pubkey must sign
    - script must be executed in the same tx
3. All new outputs to the address must contain the proper datum:
    - `swapPrice` > 0
    - `swapBeacon` == Nothing

The first requirement also means that the datum attached to the beacon's reference script cannot be updated. This is fine since that datum is never allowed in swaps anyway.

### `Swap` Redeemer
The `Swap` redeemer checks all of the assets leaving the swap address and all of the assets entering the swap address. For a successful swap, all of the following must be true:

1. No beacons among tx inputs.
2. All swap input prices are > 0.
3. All outputs to the swap address contain the proper datum:
    - `swapPrice` == weighted avg price of all swap inputs
    - `swapBeacon` == Nothing
4. Only the offered asset (as defined in `SwapConfig`) is leaving the swap address.
5. QuantityOfferedAssetTaken * weighted average price <= quantityAskedAssetGiven

Custom error messages are included to help troubleshoot why a swap failed. The weighted average price must match exactly what the swap contract calculates. To help with this, `cardano-swaps` can calculate the weighted price for you. The function `cardano-swaps` uses is the same function the on-chain swap contract uses.

---
## Composable Atomic Swaps
Thanks to being able to securely combine atomic swaps into one transaction, any arbitrarily complex swap transaction can be created. 

Do you want to convert 10 ADA into 5 DUST and 5 AGIX? No problem! This can be done in one transaction.
What about converting 10 ADA, 5 DUST, and 3 WMT into 16 AGIX and 1 of your favorite NFTs? Piece of cake!

By composing these atomic swaps, in one transaction, you can easily and securely do:
``` Haskell
many assets -> many assets
```

The only limits are the maximum transaction limits for Cardano.

---
## Liquidity
Liquidity on Cardano-Swaps is entirely due to combining properly incentivized abritrage with being able to chain (compose) swaps together into one transaction.

### The Contrived Example

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

In short, Charlie pays the transaction fee and in return receives 5 AGIX. As a bonus, both Alice's and Bob's swaps were fulfilled.

### The Realistic Example

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

In short, Sarah pays the transaction fee and in return receives 5 AGIX. As a bonus, four swaps were fulfilled.

### Liquidity naturally flows to the less liquid pairs
As you saw in the realistic example, Sarah was able to fulfill both the AGIX/DUST swap and the HOSKY/AGIX swap simply by "passing through" those pairs on her way back to ADA. As long as the entry and exit pairs (in this case ADA/HOSKY and DUST/ADA) have plenty of liquidity, arbitragers can spread that liquidity into the less liquid pairs.

Thanks to the very nature of illiquid pairs having more arbitrage opportunities, illiquid pairs are where most of the profit will be. Anyone can be an abritrager; everyone can design their own algorithms for finding the most profitable path through the available swaps.

### What if two arbitragers compete for the same swap?
Recall the contrived example above. What would happen if Charlie and Mike try to arbitrage it at the same time?

1. Charlie and Mike successfully build their transactions since the UTxOs still exist.
2. Charlie and Mike submit their transaction at the same time.
3. Charlie's is added to a block first.
4. When Mike's transaction is then picked to go into a block, the UTxOs no longer exist. The transaction fails without needing to run the swap contracts.

Since Mike's transaction will fail without needing to run the swap script, Mike's collateral is safe. Further, the more available swaps their are, the less likely these "collisions" are.

---
## Benchmarks and Fee Estimations (YMMV)
Basic benchmarking tests were done to determine the maximum amount of inputs or outputs that the spending script could handle in one transaction.

Cardano currently suffers from an issue where a spending script is executed once for every script input. So if there are 5 inputs from one of the script's addresses, the spending script will be executed 5 times. The latter 4 are completely redundant in this situation. These redundant exectutions impact the maximum number of inputs and outputs that can fit within one transaction. There is a Cardano Problem Statement that is looking to address this ([here](https://github.com/cardano-foundation/CIPs/pull/418)).

### Creating a Live Address
For opening a new address, I was successfully able to mint the beacon, store the reference script, and create 80+ new swap positions in one transaction.

| Number of New Swaps | Tx Fee |
|--|--|
| 5 | 0.647711 ADA |
| 10 | 0.697339 ADA |
| 15 | 0.747768 ADA |
| 20 | 0.797797 ADA |
| 25 | 0.847869 ADA |

### Closing a Live Address
For closing a new address, I was successfully able to burn the beacon, remove the reference script, and close 5 open positions in one transaction.

| Number of Closed Swaps | Tx Fee |
|--|--|
| 1 | 0.577408 ADA |
| 2 | 0.747233 ADA |
| 3 | 0.967701 ADA |
| 4 | 1.238813 ADA |
| 5 | 1.560569 ADA |

### Updating Open Swaps
For updating open swaps, I was successfully able to update 4 positions and recreate them in one transaction. If you consolidate your open positions into one output, you can update more in one transaction.

| Number of Swaps Updated | Tx Fee |
|--|--|
| 1 | 0.271886 ADA |
| 2 | 0.449765 ADA |
| 3 | 0.715111 ADA |
| 4 | 1.067924 ADA |

### Swapping Assets
For swapping assets, I was successfully able to chain together 3 swap utxos.

| Number of Swaps Chained | Tx Fee |
|--|--|
| 1 | 0.325983 ADA |
| 2 | 0.568143 ADA |
| 3 | 0.902922 ADA |

---
## Upgradability
Being that users can close their swaps at any time, whenever there is a potential upgrade, users can choose to close their current swaps and recreate them with the new contracts. It is very similar to how Cardano Stake Pool Operators can currently choose which version of `cardano-node` to use. Just like with upgrading the `cardano-node`, the main difficulty with upgrading beacon tokens is that enough users need to upgrade for there to be enough liquidity for the DEX. 

---
## Frontend Agnostic
By using the beacon tokens, it is trivial for any frontend to integrate with Cardano-Swaps. For example, the new Lace wallet by IOHK can integrate the DEX by simply adding support for querying the beacon tokens. They can also add their own user friendly way to create and use swaps. The only requirement is that all frontends agree to use the same beacon token standard. There is no need for risky extensions or dedicated frontends in order to use this DEX.

---
## Conclusion
This DEX protocol has all of the desired properties of a DEX:

1. Users maintain delegation control at all times.
2. Composable atomic swaps allow creating arbitrarily complex swap transactions.
3. Naturally concurrent and gets more concurrent the more available swaps there are.
4. There is no impermanent loss since users can declare their desired minimum price.
5. No secondary token is needed to interact with the DEX; only ADA is needed for the fees and deposits.
6. Upgrades can happen democratically.
7. Any frontend can easily add Cardano-Swap support.

Thanks to using beacon tokens, decentralization is no longer limited by the design of DEXs. Instead, the limiting factor is now the off-chain querying. However, innovations in this space are still in the early days. The Koios api is an example of a more decentralized off-chain platform. As the technology improves, the decentralization of this DEX will only improve.

---
## FAQ

### If all users have direct access to the spending script for that trading pair, why are reference scripts required?

The reason is to incentivize users to actually use the DEX as intended. If there were no deposits, users can create a live address with a beacon token and then just leave it open even if they aren't actually using the address anymore (like if there are no swappable assets in it). This "zombie" address will still appear when querying the beacon tokens even though other users can't actually do anything with it. By requiring the deposits, users are incentivized to close unused addresses in order to get the deposits back.