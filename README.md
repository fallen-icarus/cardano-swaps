# Cardano-Swaps

The Getting Started instructions can be found [here](./GettingStarted.md) and the benchmarks can
be found [here](./Benchmarks/).

---
## Table of Contents 
- [Abstract](#abstract)
- [Motivation](#motivation)
- [How It Works: Core Concepts](#how-it-works-core-concepts)
- [Specification](#specification)
- [Benchmarks and Fee Estimations](#benchmarks-and-fee-estimations-ymmv)
- [Protocol Discussion: Implications and Strategies](#protocol-discussion-implications-and-strategies)
- [Conclusion](#conclusion)

## Abstract

Cardano-Swaps is the DeFi Kernel's fully peer-to-peer order book settlement protocol, designed to
replace TradFi's permissioned and centralized [DTCC][1]. As a foundational settlement layer,
businesses and Layer 2 solutions are meant to build on top of it. While end-users will likely
interact with the protocol directly in its early stages, the ecosystem is designed to evolve. Over
time, users will naturally migrate to specialized L2s for the majority of their trades. The
settlement protocol, however, will remain the ecosystem's liquidity core, serving trades that demand
maximum censorship-resistance. Since large entities like central banks and pension funds prioritize
censorship-resistance over high throughput, deep liquidity will gravitate to this foundational
layer. With this in mind, the protocol has features specifically designed to allow L2s and other
DeFi applications to seamlessly tap into and share this liquidity.

## Motivation

There are three reasons why Cardano needs this order book settlement protocol:

1. **A Layered Architecture is the Only Sustainable Path to Scale.**
2. **Capital Efficiency is Non-Negotiable for Serious Finance.**
3. **Self-Sovereignty is a Prerequisite for Deep Liquidity.**

### The Imperative to Build in Layers

The rationale for this layered approach is detailed in foundational documents like [The DeFi
Hypothesis][2] and this [technical seminar][3]. The core idea is that while a single system faces a
trilemma, a composite system of specialized layers does not:

> The blockchain trilemma applies to individual entities. If two entities specialize—one for
> censorship-resistance and one for high-throughput—and then interoperate, the collective system
> completely sidesteps the trilemma. End-users are free to use the layer that best supports their
> needs.

This architectural choice has profound consequences for building a sustainable DeFi ecosystem:

- **Liquidity Gravitates to Censorship-Resistance.** A critical distinction is often missed between
trading volume and market liquidity. While high-frequency traders may dominate volume, the vast
majority of actual capital is held by long-term players like central banks and pension funds who
prioritize security and censorship-resistance above all else. A layered model allows the L1 to
become the specialized venue for these entities, ensuring it becomes the ecosystem's center of
gravity for liquidity.
- **It Solves the State Bloat Problem.** Monolithic chains are not sustainable. Despite its youth,
Solana's chain size is already in the petabyte range, creating pressure to prune its history. This
compromises its ability to serve as a permanent ledger. By separating concerns, a high-frequency
trading L2 can have its history pruned on an aggressive cadence without affecting the L1. This
preserves the L1's integrity as an immutable record, which is essential for applications requiring
long-term data like trustless credit histories.

### Capital Efficiency is Non-Negotiable for Serious Finance

The capital inefficiency of current DeFi stems directly from its reliance on the **constant-product
AMM**. This formula, while simple, is extremely inefficient because it ties an asset's price
directly to its ratio within a liquidity pool. To execute a trade of size `x` while keeping slippage
under 1%, a constant-product AMM requires `100x` that amount in passive liquidity. Zero-slippage
trades are a mathematical impossibility, requiring infinite liquidity.

An order book, in contrast, is 100% capital efficient: a trade of size `x` requires only `x` in
active liquidity at the target price to execute with zero slippage.

This is not a minor flaw; it is a systemic barrier to adoption with critical consequences:

- **It Cripples Essential Financial Functions.** A stablecoin treasury activating $5 million to
defend its peg is handicapped by the constant-product formula, which would require a $500 million
liquidity pool to be effective. An order book makes this defense feasible with just $5 million of
active bids or asks.
- **It Deters Institutional Capital.** Large entities like central banks, who may need to swap $50
million at a time, will not tolerate the value leakage caused by constant-product AMMs. They will
consistently choose centralized, order-book-based exchanges, starving DeFi of significant global
liquidity.

This doesn't mean AMMs have no future; it means their future is to evolve. The next generation of
AMMs will move away from simple formulas and instead use a deep, on-chain order book as their
primary source for price discovery.

By providing this foundational primitive, Cardano-Swaps enables a more mature and efficient DeFi
ecosystem. It allows Cardano's capital to provide the same **effective market depth** as a vastly
larger pool of capital locked in inefficient, constant-product AMMs. This is the key to unlocking
true liquidity for Cardano, and competing with the liquidity Goliath that is Ethereum's DeFi.

### Self-Sovereignty is a Prerequisite for Deep Liquidity

There is a fundamental conflict between the design of prevailing DeFi protocols and the core
principles of Cardano. The network's security (Ouroboros) and governance (on-chain democracy) are
powered by the direct delegation choices of individual ADA holders. Yet, most DeFi protocols, built
on a shared smart contract model, force users to surrender both custody of their assets and,
critically, their delegation rights.

This model poses an existential threat to Cardano's decentralization and has created the single
greatest barrier to DeFi adoption on the network. The proof is in the on-chain data: approximately
**98.5% of Cardano's potential liquidity remains on the sidelines**, held by users and institutions
unwilling to make the unacceptable trade-off between participation and sovereignty.

Cardano-Swaps resolves this conflict by design. It abandons the shared contract model. Instead,
leveraging [CIP-89][4], each user interacts through their own **individual, sovereign smart contract
instance**. This architecture guarantees that users **always maintain full custody of their assets
and complete, unabridged control over their ADA delegation rights**.

This commitment to user sovereignty is what elevates Cardano-Swaps from a mere alternative to the
DTCC into a categorical upgrade. While the DTCC provides settlement, it does so within a closed,
custodial system. Cardano-Swaps provides settlement as a public good, offering three transformative
advantages:

- **Self-Custodial:** Unlike the DTCC where your assets are held by an intermediary, on
Cardano-Swaps, you are always in control. *Not your keys, not your crypto.*
- **Permissionless:** The DTCC is a walled garden for the world's largest financial institutions.
Cardano-Swaps is open to anyone with an internet connection, from an individual in a developing
nation to a central bank.
- **Trustless:** The DTCC requires trusting a centralized entity to maintain the ledger.
Cardano-Swaps replaces this trust with cryptographic certainty and code that is open for all to
audit.

By aligning financial utility with the core principles of decentralization, Cardano-Swaps is
designed to be the trustworthy foundation required to unlock the vast pool of dormant capital on
Cardano and build a financial system that is not only more efficient but fundamentally more free.

## How It Works: Core Concepts

The Cardano-Swaps protocol is built on a set of simple, composable primitives that combine to create
a uniquely powerful and flexible settlement layer.

### The Primitives: One-Way and Two-Way Swaps

The protocol is comprised of two core swap types:

- **One-Way Swaps (Limit Orders):** These are the fundamental building blocks for directional
trades. A one-way swap can only be executed at its specified price or better, functioning exactly as
a limit order does in a traditional order book. Execution is driven by arbitrageurs who are
incentivized to fill these orders against external liquidity sources, like AMMs, once the market
price aligns. This ensures orders are filled automatically without the user needing to be online,
making the protocol a universal order book for the entire Cardano ecosystem.
- **Two-Way Swaps (Liquidity Swaps):** This is the mechanism for providing liquidity profitably
without resorting to yield farming. A two-way swap (e.g., ADA ↔ DJED) can be executed in either
direction. The liquidity provider specifies a separate price for each direction, creating a spread.
For example, a provider can set a DJED → USDC swap at a price of 1.01 USDC and a USDC → DJED swap at
a price of 1.01 DJED. This guarantees them a 1% profit regardless of which direction the swap is
executed, creating a sustainable incentive for market makers.

*(The advanced strategies for using Liquidity Swaps to market make profitably, even in a
low-frequency environment like Cardano, are explored in the **Protocol Discussion** section after the
specification.)*

### The Superpower: Unbounded Composability

The true power of the protocol emerges from the ability to compose these simple swaps into a single,
atomic transaction. This has two transformative benefits:

- **Arbitrarily Complex Swaps:** Users can chain swaps together to create novel trading paths (e.g.,
ADA -> ERGO -> DUST). This not only enables multi-asset conversions in a single transaction but also
creates a fertile ground for arbitrageurs. The profit motive for arbitrage naturally incentivizes
the distribution of liquidity across all trading pairs, preventing the siloing of liquidity into
only a few trading pairs, something common in other DEX models.
- **Shared Liquidity Across the Ecosystem:** Composability allows any dApp on Cardano to tap into
the protocol's liquidity. For instance, a user can buy an NFT priced in ADA, even if they only hold
DJED. They simply compose the NFT purchase with a DJED -> ADA swap. The entire operation is atomic:
if the NFT purchase fails for any reason, the asset swap also fails, protecting the user from
unwanted exposure.

*(The free-market mechanism for managing the UTxO contention that arises from this powerful feature
is also detailed in the **Protocol Discussion** section.)**

### The Foundation: A True Peer-to-Peer Network

Through the use of [CIP-89][4] beacon tokens, the protocol operates as a true peer-to-peer network
without requiring centralized batchers. This architecture is the key to delivering on the promise of
self-sovereignty:

- Each user is given their own unique smart contract address.
- This guarantees that users **always maintain full custody, delegation, and voting control** of
their assets.

## Specification

*(If you are only interested in the high-level aspects of the protocol, feel free to skip to the next
[section](#benchmarks-and-fee-estimations-ymmv).)*

### Core Architectural Principles

The protocol's on-chain design is modular, built around a `spending script / beacon script` pair for
each type of swap. This approach allows for easy extension with new swap types in the future. The
following principles apply to all swap types.

- **Sovereign Addresses:** All users get their own DEX addresses for storing/protecting their open
orders. Each swap type gets its own address: all of Alice's limit orders are stored in her personal
limit order address and all of her liquidity swaps are stored in her personal liquidity swap
address.
- **P2P Discovery via Beacons:** Each order is stored on-chain as a swap UTxO that contains
non-transferable beacon tokens which are minted on creation and burned on close. This allows anyone
to discover all open swaps on-chain.
- **On-Chain Efficiency:** Some validation logic is delegated from the spending script to the beacon
script (which runs only once per transaction) to minimize redundant executions and fees.
- **Owner vs. Public Actions:** Creating, updating, or closing a swap is restricted to the owner
(via staking credential approval). Executing a swap is a public action.

> [!WARNING]
> All swap datums contain beacon information that must be configured correctly upon
> creation. If incorrect beacon information is supplied, the resulting UTxO may be locked forever.

### Beacon Naming Conventions

To guarantee uniqueness and fit within Cardano's 64-character limit for token names, all beacon
names are derived by hashing concatenated asset information.

##### One-Way Swaps

A one-way swap uses three distinct beacons:

- **Offer Beacon:** `sha2_256( "01" ++ offer_policy_id ++ offer_asset_name )`
- **Ask Beacon:** `sha2_256( "02" ++ ask_policy_id ++ ask_asset_name )`
- **Pair Beacon:** `sha2_256( offer_id ++ offer_name ++ ask_id ++ ask_name )`

To distinguish between ADA → TOKEN and TOKEN → ADA, ADA's policy ID is replaced with "00" when it is
the ask asset. This ensures each direction has a unique pair beacon.

##### Two-Way Swaps

A two-way swap uses a non-directional pair beacon and two asset beacons. The assets in the pair are
first **sorted lexicographically** to determine `asset1` and `asset2`. This ensures consistency for
off-chain queries.

- **Asset1 Beacon:** `sha2_256( asset1_policy_id ++ asset1_asset_name )`
- **Asset2 Beacon:** `sha2_256( asset2_policy_id ++ asset2_asset_name )`
- **Pair Beacon:** `sha2_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )`

### Swap Primitives: Datums and Redeemers

> [!IMPORTANT]
> Datums and redeemers for one-way and two-way swaps are distinct data types, even if they share
> field names.

##### One-Way Swaps (Limit Orders)

```haskell
data SwapDatum = SwapDatum
  { beaconId    :: CurrencySymbol  -- ^ Hash of the one-way swap beacon script.
  , pairBeacon  :: TokenName       -- ^ Trading pair beacon asset name.
  , offerId     :: CurrencySymbol  -- ^ Offer policy id.
  , offerName   :: TokenName       -- ^ Offer asset name.
  , offerBeacon :: TokenName       -- ^ Offer beacon asset name.
  , askId       :: CurrencySymbol  -- ^ Ask policy id.
  , askName     :: TokenName       -- ^ Ask asset name.
  , askBeacon   :: TokenName       -- ^ Ask beacon asset name.
  , swapPrice   :: Rational        -- ^ Desired swap ratio: Ask/Offer.
  , prevInput   :: Maybe TxOutRef  -- ^ Tracks the corresponding input for an execution.
  , expiration  :: Maybe POSIXTime -- ^ An optional expiration. Must fall on 1-min interval.
  }

data BeaconPolicyRedeemer
  = RegisterBeaconScript -- ^ Register beacon script for staking execution.
  | CreateOrCloseSwaps   -- ^ Executes the beacon script as a minting policy.
  | UpdateSwaps          -- ^ Executes the beacon script as a staking script.

data SwapSpendingRedeemer
  = SpendWithMint  -- ^ Delegates checks to the beacon script's minting policy execution.
  | SpendWithStake -- ^ Delegates checks to the beacon script's staking script execution.
  | Swap
```

- **Owner Actions:** `SpendWithStake`, `SpendWithMint`
- **Public Actions:** `Swap`

##### Two-Way Swaps (Liquidity Swaps)

```haskell
data SwapDatum = SwapDatum
  { beaconId     :: CurrencySymbol -- ^ Hash of the two-way swap beacon script.
  , pairBeacon   :: TokenName      -- ^ Trading pair beacon asset name.
  , asset1Id     :: CurrencySymbol -- ^ Policy id for the first asset in the sorted pair.
  , asset1Name   :: TokenName      -- ^ Asset name for the first asset.
  , asset1Beacon :: TokenName      -- ^ Beacon name for asset1.
  , asset2Id     :: CurrencySymbol -- ^ Policy id for the second asset.
  , asset2Name   :: TokenName      -- ^ Asset name for the second asset.
  , asset2Beacon :: TokenName      -- ^ Beacon name for asset2.
  , asset1Price  :: Rational       -- ^ Price to take asset1 (Asset2/Asset1).
  , asset2Price  :: Rational       -- ^ Price to take asset2 (Asset1/Asset2).
  , prevInput    :: Maybe TxOutRef -- ^ Tracks the corresponding input for an execution.
  , expiration  :: Maybe POSIXTime -- ^ An optional expiration. Must fall on 1-min interval.
  }

data BeaconPolicyRedeemer
  = RegisterBeaconScript -- ^ Register beacon script for staking execution.
  | CreateOrCloseSwaps   -- ^ Executes the beacon script as a minting policy.
  | UpdateSwaps          -- ^ Executes the beacon script as a staking script.

data SwapSpendingRedeemer
  = SpendWithMint  -- ^ Delegates checks to the beacon script's minting policy execution.
  | SpendWithStake -- ^ Delegates checks to the beacon script's staking script execution.
  | TakeAsset1     -- ^ Take asset 1 from the swap and give asset 2.
  | TakeAsset2     -- ^ Take asset 2 from the swap and give asset 1.
```

- **Owner Actions:** `SpendWithStake`, `SpendWithMint`
- **Public Actions:** `TakeAsset1`, `TakeAsset2`

### Swap Execution Logic (Public Actions)

Any user can execute an open swap as long as the conditions are met.

##### High-Level Process

The spending script is executed for each swap input in a transaction. Its logic is to:

1. Identify the corresponding output UTxO at the same address, using the input's `pairBeacon` and
   updating the `prevInput` field in the output datum.
2. Compare the asset values between this input/output pair.
3. Validate that the exchange meets the price defined in the datum. **Partial fills are allowed.**
4. Checks the swap is not expired.

This design cleverly utilizes Cardano's per-UTxO script execution to enable cheap composition of
swaps across different trading pairs within a single transaction.

> [!NOTE]
> Since the spending script first checks for the trading pair beacon, each execution is dedicated to
> a specific trading pair. Any other outputs are ignored in this specific execution. This logic
> works because a script is executed once for every UTxO spent from the address. If input 1 is for
> beacon XYZ and input 2 is for beacon ABC, the first execution can be dedicated to beacon XYZ and
> the second execution can be dedicated to ABC. The net transaction will only succeed if all
> executions succeed. This behavior allows cheaply composing swaps of different trading pairs that
> are located at the same address. In other words, the design is taking advantage of the redundant
> executions.

##### Low-Level Validation Rules

1. **Valid Input:** The input UTxO must contain its correct trading pair beacon, ensuring its datum
   is valid.
2. **Valid Output:** A single corresponding output must exist at the same address, containing the
   same beacons and assets as the input, with the exception of the assets being swapped and ADA. Its
datum must match the input's, except `prevInput` is updated to the `TxOutRef` of the input.
3. **Price Condition:** The ratio of `asked asset given / offered asset taken` must be ≥ the price
   specified in the datum.
4. **Asset Purity:** Only the offered asset may leave the UTxO, and only the asked asset may be
   deposited (ADA can always be deposited to meet min-UTxO requirements).
5. **Expiration Check:** If the datum contains an expiration time, the transaction's validity
   interval must be bounded by this time. The transaction's `invalid-hereafter` slot must be set and
must be less than or equal to the expiration time.

### Owner Actions (Creating, Updating, Closing)

The process for owners is consistent across all swap types and is primarily validated by the beacon
script.

##### Creating a Swap

To create a swap, the owner must use the `CreateOrCloseSwaps` beacon redeemer. This requires:

1. The beacon script is executed as a **minting policy**.
2. The minted beacons are sent to the correct universal swap address, which must have a valid
   staking credential.
3. The output UTxO contains exactly one of each required beacon type for the swap and no extraneous
   assets (besides ADA).
4. The output UTxO has a valid inline `SwapDatum` with `swapPrice > 0`.
5. The assets in the pair are distinct.
6. *For two-way swaps:* `asset1` must be lexicographically less than `asset2`.
7. *Optional Expiration:* If an expiration time is set, it must be a multiple of 60,000 (a 1-minute
   interval). The transaction creating the swap must also have its `invalid-hereafter` field set
less than or equal to this time.

This process requires a deposit of **~2 ADA** per swap UTxO, which is reclaimable upon closing.

> [!NOTE]
> **On 1-Minute Expiration Intervals**
>
> The requirement for expirations to fall on a 1-minute interval is a deliberate design choice. As
> this protocol is a settlement layer and not a high-frequency trading (HFT) venue, sub-second
> precision is unnecessary. This interval strikes a crucial balance: it is slow enough for light
> wallets to reliably query and keep up with the state of the on-chain order book, yet granular
> enough to enable advanced, non-HFT trading strategies.

##### Updating or Closing a Swap

The owner's approval (via the staking credential) is required.

- **To update prices only:** Use the `UpdateSwaps` beacon redeemer (executed as a staking script)
and the `SpendWithStake` spending redeemer. This is the most efficient method when no beacons are
being minted or burned.
- **To close a swap or change pairs:** Use the `CreateOrCloseSwaps` beacon redeemer (executed as a
minting policy) and the `SpendWithMint` spending redeemer. This allows for burning old beacons and
minting new ones. To reclaim the ~2 ADA deposit, the beacons **must be burned**.

> [!IMPORTANT]
> When spending multiple swap UTxOs as the owner, use the same redeemer combination for all of them.
> If even one swap is being closed, use the `CreateOrCloseSwaps` combination for all inputs to avoid
> redundant script executions and save on fees.

## Benchmarks and Fee Estimations (YMMV)

The protocol is capable of handling 25 swaps in a single transaction, regardless of the composition
of one-way and two-way swaps in the transaction.

**No CIPs or hard-forks are needed. This protocol works on the Cardano blockchain, as is.**

Full benchmarking details can be found in the [Benchmarks](./Benchmarks/) folder.

## Protocol Discussion: Implications and Strategies

This section explores some of the deeper implications of the Cardano-Swaps design and provides
insight into advanced strategies for interacting with the protocol.

### Foundational Protocol Properties

##### Self-Sovereign DeFi

Since users get their own DEX addresses which use their own staking credentials for spending
authorization, users maintain full custody and delegation control of their assets at all times while
using the DEX. *Not your keys, not your crypto.*

##### Democratic, Non-Disruptive Upgrades

Upgrades can propagate through the ecosystem democratically. Users can choose to close their current
swaps and recreate them with new, upgraded contracts at any time. Because of the protocol's
universal composability, there is no risk of bifurcating liquidity between different versions.

##### Inherent Resistance to Denial-of-Service

Cardano's minUTxOValue requirement (~2 ADA deposit per swap) provides a natural defense against
spam. Creating millions of fake swap UTxOs to disrupt queries would require millions of ADA in
deposits. This, combined with trivial on-chain checks for asset authenticity (minting history) and
the transaction fees required to cycle the UTxOs, makes large-scale DoS attacks economically
impractical.

> [!NOTE]
> The `minUTxOValue` requirement is a core feature of Cardano's security and is not at risk of being
> removed.

### The Economic Engine: Market Roles and Trading Strategies

The protocol's liquidity and efficiency emerge from the interplay of two key roles: Market Makers
and Arbitragers.

##### The Role of the Market Maker

A market maker's primary strategy is to use Two-Way Swaps to provide liquidity and earn a profit
from the spread.

> **Strategy**
>
> To manage risk on a blockchain with slower block times, market makers should not compete on speed
> but instead **price in the time-based risk**. A sophisticated provider will calculate the expected
> price volatility over a transaction's confirmation window (e.g., 5-10 blocks) and set their spread
> wider than this value. The spread becomes the premium earned for accepting short-term volatility,
> enabling profitable market making without high-frequency updates.

This strategy transforms the risk of Cardano's slower block time into a manageable business
parameter. 

##### The Role of the Arbitrageur

Arbitrage is the connective tissue of the protocol. Arbitrageurs perform three vital, profit-driven
functions that benefit the entire ecosystem:

1. **Executing Limit Orders Against AMMs:** The order book is useful from day one because its
   One-Way Swaps (limit orders) are filled by arbitrageurs. When the price on an external AMM
crosses a user's limit order price, an arbitrageur is incentivized to simultaneously trade with
both, capturing the spread. This profit-seeking action is what executes the user's limit order,
making Cardano-Swaps a universal order book for all of Cardano.
2. **Spreading Liquidity Across Pairs:** Arbitrageurs connect liquidity within the protocol. By
   chaining multiple swaps together (e.g., ADA -> HOSKY -> AGIX -> DUST -> ADA) in a single atomic
transaction, they can profit from price discrepancies. This act of routing connects otherwise
isolated pools of liquidity, ensuring that liquidity is not siloed but is fluidly accessible across
the entire ecosystem.
3. **Acting as On-Demand Market Order Batchers:** Instead of using a custodial AMM for a market
   order, a user can hire an arbitrageur on-demand. By creating a One-Way Swap intentionally priced
to be an immediate, risk-free arbitrage opportunity against a major AMM (e.g., offering to sell ADA
for 1.99 DJED when the AMM price is 2.0), the user creates a "negative spread." The arbitrageur's
incentive is to fill this order instantly to capture the guaranteed profit, effectively acting as
the user's personal, non-custodial batcher.

### Advanced Applications

##### The UTxO Contention Market

The ability to compose any swap into a transaction creates a free-market solution to UTxO
contention. Rather than competing for the single best-priced swap, a user needing guaranteed
execution can "pay up" by selecting a swap with a slightly worse price, where contention is
exponentially lower. This allows users to pay a small premium for predictable, reliable execution.

##### L2 and dApp Interoperability

This contention market is the key to secure interoperability. An L2 or dApp needing to atomically
swap assets can "pay up" for a low-contention UTxO to ensure their transaction succeeds. This means
L1 market makers can also profit directly from providing liquidity to the entire L2 and dApp
ecosystem on Cardano.

## Conclusion

Cardano-Swaps delivers a foundational settlement layer for the digital age, but it is more than just
a decentralized alternative to TradFi's DTCC. While the DTCC provides settlement within a closed,
custodial, and permissioned system, Cardano-Swaps offers settlement as a public good, presenting
three transformative advantages: it is **self-custodial**, entirely **permissionless**, and
cryptographically **trustless**.

Furthermore, it is protocol designed for the realities of a public blockchain, transforming core
challenges like UTXO contention from intractable problems into predictable, free-market mechanisms
for guaranteeing settlement.

By providing the trustworthy foundation required to unlock the vast pool of dormant capital on
Cardano, it paves the way for a financial system that is not only more efficient and composable but
fundamentally more free.

[1]: https://en.wikipedia.org/wiki/Depository_Trust_%26_Clearing_Corporation
[2]: https://github.com/fallen-icarus/meditations-blog/blob/main/The%20DeFi%20Hypothesis/README.md
[3]: https://youtu.be/Pk6eNMLNDps
[4]: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0089/README.md
[5]: https://github.com/cypher-enterprises/p2p-audit/blob/main/audit.pdf
[6]: https://github.com/fallen-icarus/cardano-swaps/tree/9ec41e7619f5ba9d3dd46dd194e2146098093721
