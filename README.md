# Cardano-Swaps

:warning: Knowledge of basic Haskell syntax and cardano-cli usage is assumed

The Getting Started instructions can be found [here](GettingStarted.md).

---
## Table of Contents
- [Motivation](#motivation)
  - [Liquidity Pools](#liquidity-pools)
  - [Programmable Swaps](#programmable-swaps)
  - [Cardano-Swap DEX](#the-cardano-swaps-dex)
- [Personal Swap Contracts](#personal-swap-contracts)
  - [SwapConfig](#swap-config)
  - [Delegating](#delegating)
  - [Price Inline Datum](#the-price-inline-datum)
  - [Swap Contract Logic](#the-swap-contract-logic)
- [Beacon Tokens](#beacon-tokens)
  - [Beacon Tokens with Cardano-Swaps](#beacon-tokens-with-cardano-swaps)
  - [How Do Beacon Tokens Differentiate Between Trading Pairs?](#how-do-beacons-differentiate-between-trading-pairs)
  - [Defending Against Beacon Abuse](#defending-against-beacon-abuse)
  - [Beacons with reference scripts](#beacons-with-reference-scripts)
  - [Generalizing Beacon Tokens](#generalizing-beacon-tokens)
- [Composable Atomic Swaps](#composable-atomic-swaps)
- [Liquidity](#liquidity)
  - [The Contrived Example](#the-contrived-example)
  - [The Realistic Example](#the-realistic-example)
  - [Liquidity Naturally Flows To The Less Liquid Pairs](#liquidity-naturally-flows-to-the-less-liquid-pairs)
  - [What If Two Arbitragers Compete For The Same Swap?](#what-if-two-arbitragers-compete-for-the-same-swap)
  - [Transaction Fee Estimation For Chaining Swaps](#transaction-fee-estimation-for-chaining-swaps)
- [Upgradability](#upgradability)
- [Frontend Agnostic](#frontend-agnostic)
- [Conclusion](#conclusion)
- [Potential Adaptations of the Design](#potential-adaptations-of-the-design)

---
## Motivation
Any DEX that operates on a Proof-of-Stake (PoS) blockchain **MUST** include delegation control as a foundational feature for the protocol. It is existentially important for the long-term sustainability of the underlying blockchain. Yet all Cardano DEXs either cannot by design or delegation control is just an afterthought.

### Liquidity Pools
The most commonly used DEX design is that of using liquidity pools. These have the well know downsides of:
 
  1. Impermanent Loss
  2. Loss of Delegation Control
  3. Difficulty of Concurrency

Each of these downsides have their own corresponding "solution": yield farming, governance tokens, and batchers, respectively. However these "solutions" have downsides of their own:
 
  1. The long term sustainability of yield farming is questionable.
  2. Fairly distributing governance tokens is difficult.
  3. Batchers are effectively middle-men that can take advantage of their position between users and the protocol.

Even more concerning is that, even if governance tokens are fairly distributed, this doesn't change the fact that a liquidity pool can only be delegated to one stake pool. Fractionalizing liquidity pools help but it still results in a more centralized PoS blockchain than if each user was able to delegate their assets independently.

### Programmable Swaps
Programmable Swaps are a more promising design than liquidity pools and were first proposed by Axo (formerly known as Maladex). However, in Axo's [whitepaper](https://www.axo.trade/whitepaper.pdf), there is no mention of giving users full delegation control of their own assets. When they were asked on discord about it, they said it was possible but delegation control would not be included in the first version.

### The Cardano-Swaps DEX
Cardano-Swaps took inspiration from Axo's programmable swaps design but added delegation control as a foundational feature. `cardano-swaps` is the name of the CLI program included to help use the DEX.

Interestingly, by starting with the requirement of delegation control, other nice properties naturally emerged:

  1. Composable atomic swaps
  2. Naturally concurrent and gets more concurrent the more users there are. No batchers are required.
  3. Liquidity naturally spreads to all trading pairs instead of being siloed into specific trading pairs.
  4. There is no impermanent loss.
  5. ADA is all you need to interact with the DEX.
  6. Upgradability is extremely easy and doesn't introduce any security holes.
  7. Easy to integrate into any Cardano wallet software.

In addition to these nice properties, the novel use of *Beacon Tokens* can be generalized to any Decentralized Finance (DeFi) application on Cardano.

---
## Personal Swap Contracts
In order for users to maintain full delegation control of their assets, it is required for user assets to be kept siloed. The reason for this is that, on Cardano, all ADA at an address is delegated entirely or not at all (atomic delegation is not possible). So for a user to maintain delegation control of their assets, only that user's assets should be at the address he/she is using.

### Swap Config
Siloing is accomplished by passing an extra parameter to the swap plutus contract before officially compiling it. Here is the extra parameter's data type:

``` Haskell
data SwapConfig = SwapConfig
  {
    swapOwner :: PaymentPubKeyHash,
    swapOffer :: (CurrencySymbol,TokenName),  -- ^ CurrencySymbol is the policy id of the token
    swapAsk :: (CurrencySymbol,TokenName)
  }
```

Every possible `SwapConfig` will have its own unique swap contract and swap address. So if Alice and Bob both use the same `swapOffer` and `swapAsk` but have different `swapOwner`s (Alice is the owner of hers while Bob is the owner of his), Alice and Bob would have different swap contracts and swap addresses. Thanks to the fact payment pubkey hashes are cryptographically guaranteed to be unique, every user is guaranteed to have their own personal swap contract and swap address. In addition to this, every trading pair will result in a unique swap contract and swap address due to `CurrencySymbol` also being a cryptographic hash.

**All swap contracts are exactly the same except for this `SwapConfig`.**

### Delegating
With each user having their own swap address, delegating his/her swap assets is trivial:

``` Bash
cardano-cli address build \
  --payment-script-file aliceSwap.plutus \
  --stake-verification-key aliceStaking.vkey \
  --testnet-magic 1 \
  --out-file aliceSwap.addr
```

The usage is almost identical to building a traditional payment address. All assets at the `aliceSwap.addr` address are now delegated according to `aliceStaking.vkey`. The `cardano-swaps` CLI also provides the option of using a staking plutus contract to allow greater flexibility. The staking plutus script also takes a similar config parameter to guarantee unique staking contract and staking addresses:

``` Haskell
data StakingConfig = StakingConfig
  { stakeOwner :: PaymentPubKeyHash
  , stakeOfferedAsset :: Maybe (CurrencySymbol,TokenName)
  , stakeAskedAsset :: Maybe (CurrencySymbol,TokenName) 
  }
```

Once again, every possible `StakingConfig` will result in a unique staking contract and staking address. **All staking contracts are exactly the same except for this `StakingConfig`.** The logic for the staking contract is very simple: **Delegating and withdrawing rewards are only possible if the `stakeOwner` signs the transaction.** The unique thing about using the staking contract is that staking related actions are now protected by the user's payment private key as opposed to the staking private key (the staking contract must still be a witness in any staking related transactions for the user). The staking contract was provided in case a user wanted a simplified way of breaking up the delegation of their swap addresses without needing to manage any more keys. Here is how one can build a swap address using a staking contract:

``` Bash
cardano-cli address build \
  --payment-script-file aliceSwap.plutus \
  --stake-script-file aliceStaking.plutus \
  --testnet-magic 1 \
  --out-file aliceSwap.addr
```

### The Price Inline Datum
All utxos at a swap address are assumed to have the proper inline datum for a price. A swap is guaranteed to fail if the utxo being used does not have a valid price inline datum. The actual data type is this:

``` Haskell
-- | A fraction with an Integer for both the numerator and denominator
type Price = Rational  -- ^ Always quantityAskedAsset/quantityOfferedAsset
```

On-chain, this inline datum looks like this:

``` Txt
(ScriptDataConstructor 0 [ScriptDataNumber 3,ScriptDataNumber 2])
```

The first `ScriptDataNumber` is the numerator and the second `ScriptDataNumber` is the denominator. The `ScriptDataConstructor` number can be ignored, it will always be `0`. In this example, the asking price is 1.5. This price means you must deposit 1.5 of the asked asset for every 1 of the offered asset you take. 

Fractions are used on-chain due to the inability to properly use decimal types on-chain. The good news is there is no loss of functionality from using fractions. Users are able to supply decimals to `cardano-swaps` and it will properly convert the decimal to `Rational`.

Prices set to zero or a negative ratio mean the assets are effectively free. To prevent this and a possible attack, swaps will fail unless all prices are greater than zero. `cardano-swaps` checks whether the supplied price is greater than 0 so it is recommended to rely on `cardano-swaps` CLI when creating datums for the DEX.

When ADA is part of the pair, the price **MUST** be in units of ADA. The swap contract will correctly convert to lovelace if necessary. This was to improve usability. If you accidentally provide the price in terms of lovelace, the price will still undergo the conversion when the script executes and produce undesired behavior.

By using prices like this, no oracles are needed for this DEX. Every user can set their own desired swap ratio. This is very similar to how order books, on centralized exchanges, are just the sorted limit orders between bids and asks. The "global" price naturally emerges where the local bids and asks meet.

### The Swap Contract Logic
Swap contracts have four possible actions, aka redeemers:

1. `Info` - display the owner's payment pubkey hash
2. `Close` - withdraw any utxo located at the swap address
3. `UpdatePrices` - update the asking price for the utxos at the swap address
4. `Swap` - try swapping with assets at the swap address

Only the owner (as defined by `SwapConfig`) is allowed to use the `Close` or `UpdatePrices` redeemers. Anyone can use the `Info` or `Swap` redeemers.

#### `Info` Redeemer
The `Info` redeemer is guaranteed to fail and will display the `swapOwner` of the `SwapConfig` in the produced error message. It is guaranteed to fail at the `cardano-cli transaction build` step so there is no risk of losing collateral. This option was added in case someone wanted to check if the swap is using the proper swap contract. Since you will know the offered asset and asked asset (thanks to beacon tokens discussed later), armed with the owner's payment pubkey hash, you can now try recreating the swap contract and swap address. If you end up with a different swap address, the swap contract you are trying to use is guaranteed to be different. In short, this option adds an auditability feature to Cardano-Swaps.

#### `Close` Redeemer
The `Close` redeemer makes it possible for the owner (as defined by `SwapConfig`) to withdraw all assets from the swap address, this includes any utxos with reference scripts. This option also allows the owner to selectively "close" swap positions at his/her address by removing them from the swap address.

The purpose of this redeemer is to also allow the user to stop their swap address from being executable by removing his/her reference script. This would close all swap positions, even if there are utxos still at the swap address. This is also how the deposit is recovered (since it is stored with the reference script).

In the case the user would like to completely close their swap address by removing his/her reference script, there is also a check to make sure the beacon is burned. This is to prevent "dead" swaps from being broadcasted to other users.

#### `UpdatePrice` Redeemer
As previously mentioned, Cardano-Swaps uses inline price datums. However, sometimes the owner will want to change the asking price of his/her positions. This redeemer allows the owner to change the inline price datum attached to his/her utxos. This action includes checks to ensure the new datum is properly used. The requirements for a successful update are:

1. The transaction must be signed by owner.
2. The new price must be greater than zero.
3. All datums must be inline datums of a price.
4. All datums must match the price passed with the redeemer (this is for more efficient checks).
5. All transaction outputs must either go to the originating swap address or the swap owner's address.
6. No reference script utxos are updated.

The last requirement might seem strange but this is to minimize transaction fees. The `Swap` redeemer ensures no reference script utxos are consumed so there is no risk in leaving the datum attached to reference script utxos alone.

#### `Swap` Redeemer
The `Swap` redeemer checks all of the assets leaving the swap address and all of the assets entering the swap address. For a successful swap, all of the following must be true:

1. No reference script utxos are consumed.
2. Only the offered asset (as defined in `SwapConfig`) is leaving the swap address.
3. All utxos being locked at the script address contain the weighted average price of all utxos leaving the swap address. 
4. The weighted average price is supplied as an inline datum.
5. QuantityOfferedAssetTaken * weighted average price <= quantityAskedAssetGiven

Custom error messages are included to help troubleshoot why a swap failed. The weighted average price supplied as the datum must match exactly what the swap contract calculates. To help with this, `cardano-swaps` allows calculating the weighted price from a JSON file. The function `cardano-swaps` uses is the same function the on-chain swap contract uses.

---
## Beacon Tokens
Upon reading how all user assets are siloed, it is natural to ask how potential "swappers" can find all available swaps and their associated reference scripts. The answer is to use beacon tokens.

Using [Koios](https://api.koios.rest/#overview) or [Blockfrost](https://docs.blockfrost.io/) apis, it is possible to find all addresses that contain a specific native token. Once you have the address, you can then use Koios or Blockfrost again to get all utxos locked at those addresses. These native tokens act as beacons which is where *Beacon Token* comes from.

| Task | Koios Api | Blockfrost Api |
|--|--|--|
| Addresses with a beacon | [api](https://api.koios.rest/#get-/asset_address_list) | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1addresses/get)|
| UTxOs at the address | [api](https://api.koios.rest/#post-/address_info) | [api](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos/get)|

The utxos api also returns which utxos contain reference scripts. This is how users can remotely execute swap contracts.

### Beacon Tokens with Cardano-Swaps
When a user creates a new swap address, the "creation" is not complete until:

1. The swap contract is stored as a reference script inside the swap address.
2. The proper beacon token is minted AND stored in the same utxo as the reference script.

The use of reference scripts is why the `Swap` redeemer protects against consuming them. By storing the beacon with the reference script, the beacon token is also protected by default against accidentally removing them with the UpdatePrice redeemer. Once these two steps are completed, the swap address is now easily discoverable by other users.

Likewise, "closing" a swap position is not complete until:

1. The reference script is consumed.
2. The beacon token is burned.

Consuming the reference script prevents anyone from remotely interacting with a swap address and burning the token makes that address "undiscoverable".

### How Do Beacons Differentiate Between Trading Pairs?
Just like all native tokens, beacon tokens have two fields:

1. The currency symbol (policy id)
2. The token name

All beacons used by Cardano-Swaps have the same policy id. The token name is how the trading pair information is captured. 

A naive approach would be to use the token names of the trading pair, like "AGIX/ADA", for the beacon token name. However, two different native tokens can technically have the same token names. For example, imagine you had the following native tokens:

``` Haskell
token1 = Asset
  { currencySymbol = "abc123"
  , tokenName = "DUST"
  }

token2 = Asset
  { currencySymbol = "def456"
  , tokenName = "DUST"
  }

token3 = Asset
  { currencySymbol = "abc123"
  , tokenName = "AGIX" 
  }
```

If you saw the trading pair of "DUST/AGIX", which DUST token name does this refer to: `token1` or `token2`? This is why only using the token name is not enough. A better solution would be to also include the currency symbols, like "abc123.DUST/abc123.AGIX". Now you can easily tell which tokens are included in the pair. 

Using the currency symbols too creates another problem: the amount of ADA that needs to be stored with native tokens is directly correlated to the length of the native token's token name. So while our above contrived example has short names, a real example would look like this:

`c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a/c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a`

This token name is way too long. To solve this problem, `cardano-swaps` takes this string and hashes it with sha256. Thus the above monster token name becomes `19858cb2eaef2ca2c67175b771890debb04a3c3538e5fc495ed312f48813f6871` which is much more managable. And once again, thanks to the cryptographic hash, every trading pair is guaranteed to have a unique beacon token name. While you can always create the token name yourself, `cardano-swaps` is able to do this for you.

To test this yourself on Linux, you can do:
``` Bash
$ echo -n "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a/c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a" | openssl dgst -sha256
```

**Just like the price datum, the beacon token name is always askedAsset/offeredAsset.**

### Defending Against Beacon Abuse
While using the Koios and Blockfrost apis are technically off-chain, when there are hundreds of thousands of address with beacons, the apis can become too slow to use. Thus use of the beacon tokens must be guarded to prevent a denial-of-service attack.

To this end, minting beacon tokens require a 2 ADA deposit to a beacon vault address which can be redeemed upon burning the beacon token. The beacon vault address is hardcoded into the beacon minting policy which means it can tell whether the necessary deposit has been made. Beacon minting will only succeed if:

1. Exactly 2 ADA is deposited to the beacon vault address.
2. Only one beacon token is minted in the transaction.
3. The proper datum is attached to the 2 ADA deposit.

The datum for the 2 ADA deposit is the inline currency symbol for the beacon token. This is necessary for the beacon vault to only allow redeeming deposits if the correct beacon token is burned. Burning beacons requires:

1. Withdrawing exactly 2 ADA from the beacon vault address.
2. Burning only one beacon token in the transaction.

By hardcoding the beacon vault address into the beacon policy and requiring the beacon currency symbol to be used as the datum for the beacon vault, the beacon policy and beacon vault are inextricably linked together.

### Beacons with reference scripts
To minimize transaction fees, the beacon policy and beacon vault script can be used as reference scripts. The beacon vault script has logic to protect any reference scripts stored in the vault from being consumed. This means both the beacon policy and the beacon vault script can be stored inside the beacon vault.

**There is no way to withdraw ADA from the beacon vault when it is stored with a reference script. Thus storing reference scripts inside the beacon vault effectively means burning the associated ADA.** There was no way to allow withdrawing the ADA without opening up centralization problems.

### Generalizing Beacon Tokens
By simply adding an extra parameter to the `mkBeaconVault` function in the [source code](src/CardanoSwaps.hs#L263), a completely unique beacon policy and beacon vault contract pair can be created. This allows different DeFi applications to use their own beacon tokens. This extra parameter can be a simple string like "cardano-swaps-dex". The beacon policy and vault pair used by Cardano-Swaps was created by using "cardano-swaps-dex-testing", seen [here](src/CardanoSwaps.hs#L331).

While these beacons are used to broadcast all necessary information for remotely executing swaps (reference script utxos and available swap utxos), they can be used for broadcasting ANY information tied to:

1. The address they are inside
2. The utxo they are stored in
3. The transaction history of the beacon. 

For example, the last use case allows the beacons to be used to also broadcast the metadata of the last transaction they were part of. This feature can create a trustless "metadata history" trail. Here is the Blockfrost [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1transactions/get) that can give you the transaction history for a beacon. Here is the Koios [api](https://api.koios.rest/#get-/asset_txs) for the same thing. This same technique can also be used to trustlessly check a datum history. 

All of this information is broadcasted automatically and trustlessly by the beacon tokens; no configuration of the tokens is necessary. You can simply decide which information to use.

---
## Composable Atomic Swaps
Thanks to being able to securely combine atomic swaps into one transaction, any arbitrarily complex swap transaction can be created. 

Do you want to convert 10 ADA into 5 DUST and 5 AGIX? No problem! This can be done in one transaction.
What about converting 10 ADA, 5 DUST, and 3 WMT into 16 AGIX and 1 of your favorite NFTs? Piece of cake!

By composing these atomic swaps, in one transaction, you can easily and securely do:
``` Haskell
many assets -> many assets
```

The only limit is the maximum transaction size for Cardano.

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

### Transaction Fee Estimation For Chaining Swaps
``` Txt
fee = # ref scripts executed * ( 0.3 ADA + 0.02 ADA * ( # input utxos + # output utxos ) )
``` 
The transaction fee increases linearly for every utxo (inputs + outputs) in the transaction, then quadratically for every reference script that needs to be executed. The reason for this is that the script must traverse all of the inputs and all of the outputs every time it is executed. 

Currenctly, the reference script must be executed once for every utxo coming from that script address. Since the swap script validates based off the transaction as a whole and not based off any individual utxo, the extra executions for each utxo coming from the same swap address are completely redundant. There is a Cardano Problem Statement (CPS) [pull request](https://github.com/cardano-foundation/CIPs/pull/418) that looks to address this issue.

---
## Upgradability
Being that users can close their swaps at any time, whenever there is a potential upgrade, users can choose to close their current swaps and recreate them with the new contracts. Users are able to use different versions of the contracts (assuming the new logic allows it) thanks to the beacon tokens still being able to link swaps across the blockchain. 

Upgrading the beacons would functions exactly the same way. It is very similar to how Cardano Stake Pool Operators can currently choose which version of `cardano-node` to use. The only difficulty with upgrading beacon tokens is that enough users need to upgrade for there to be enough liquidity for the DEX.

---
## Frontend Agnostic
By using the beacon tokens, it is trivial for any wallet to integrate with Cardano-Swaps. For example, the new Lace wallet by IOHK can integrate the DEX by simply adding support for querying the beacon tokens. They can also add their own user friendly way to create and use swaps. The only requirement is that all frontends agree to use the same beacon token standard (the same policy id and the same way of generating beacon token names). There is no need for risky extensions or dedicated frontends in order to use this DEX.

---
## Conclusion
This DEX protocol has all of the desired properties of a DEX:

1. Users maintain delegation control at all times.
2. Composable atomic swaps allow creating arbitrarily complex swap transactions.
3. Naturally concurrent and gets more concurrent the more available swaps there are.
4. There is no impermanent loss since users can declare their desired minimum price.
5. No secondary token is needed to interact with the DEX, only ADA is needed for the fees and deposits.
6. Upgrades can happen easily and democratically. Plus maintaining backwards compatibility is easy.
7. Any wallet can easily add Cardano-Swap support without opening up security holes in their software.

---
## Potential Adaptations of the Design
Since this approach gives each user a unique spending script per trading pair, each user must store each script on-chain for other people to find and use. This means the user is required to "deposit" about 20 ADA for every script they are using. While this has benefits such as disincentivizing creating empty swap addresses, the deposit may be considered steep.

### Adaption 1
Another potential approach is to have a universal spending script for each trading pair. Users would still get unique addresses by building the actual swap address with the user's own personal staking key or staking script. This still ensures full delegation control while using the DEX and all of the trading pairs would still be siloed into separate addresses.

#### Drawbacks
Since the spending script would no longer have information on the owner, this would make securing `Close` and `UpdatePrice` actions more difficult. Datums would likely be needed to compensate but since datums are mutable, this would make the DEX less secure by default. The DEX's security would now heavily depend on ensuring the integrity of the datums.

### Adaptation 2
Instead of giving each user the a universal spending script for each trading pair (like in Adaptation 1), each user can be given one unique spending script that would work on all trading pairs. Since each user has a unique spending script, their addresses would also be unique which means full delegation control is still ensured. All trading pairs would now be grouped together into the same swap address.

#### Drawbacks
Since the trading pair utxos are all grouped together into the same address, there needs to be a way to differentiate which utxos are for which trading pairs. You could give every utxo for a specific trading pair the corresponding beacon token but this would require a 2 ADA deposit for each utxo. This would quickly cancel out the gains from not having the user store their own reference scripts on-chain.

Another problem is that this puts a much larger load on the off-chain use of beacons. How much sorting would need to be done to filter out utxos of the wrong pairs? With 10000 users, you could be looking at sorting at least 10000 utxos each query, regardless of the trading pair. Would Koios and Blockfrost even be able to handle this level of querying without causing more centralization of these apis? The maximum amount of work possible would need to be done for every query, no matter how many utxos there actually are for a specific trading pair. Segregating trading pairs into separate addresses dramatically minimizes the amount of sorting required off-chain and the load on the apis.

Having all the trading pairs together also makes it harder to write an easily composable atomic swap since you know need to account for not having all utxos pre-sorted into the respective trading pairs. This extra complexity could easily lead to security holes in the DEX.