# Cardano-Swaps Protocol Versions

This document provides the official specification for each version of the Cardano-Swaps smart
contract protocol.

A new protocol version (e.g., v1, v2) is released **only when the on-chain smart contract code is
changed**. Each version corresponds to a unique, immutable set of smart contracts.

---

## Protocol v2 - *In Development*

This version makes the following changes to both one-way and two-way swaps:

- Introduces the optional order `expiration` feature.
- Beacon names are now derived from the `sha2_256` hash of the CBOR-serialised asset/pair
  constructors, making the naming scheme injective.
- The staking credential of a new swap's address must approve the creating transaction, so swaps
  cannot be accidentally created at an address whose staking credential is unusable.

-   **Status:** 🟡 **LIVE but not audited**
-   **Plutus Version:** Plutus V3
-   **Commit Hash:** [`0b24fc374c8b30ca5f46b70ab4e078cdd7333e2f`](https://github.com/fallen-icarus/cardano-swaps/commit/0b24fc374c8b30ca5f46b70ab4e078cdd7333e2f)
-   **Script Hashes:**
    - **One-Way:**
        -   Swap: `ef69e7b2174184c1a1e140f255af81bb6a8daf7d3796563ec7bdeccb`
        -   Beacon Policy: `4557249e92a42c371f494c32fcfbb31648ef14c4fb69056e56269af3`
    - **Two-Way:**
        -   Swap: `81bd68c4428281814bb2c69d75af4bc45876dfdc0af82c1ed4b8a8b4`
        -   Beacon Policy: `ca68d83fa7afe2dab5bfdaa9ee2fd5e0dc584f0d5cbbac887c2b77a2`

---

## Protocol v1 - *Current*

This version includes the core one-way and two-way swap logic without order expirations.

-   **Status:** ✅ **Live & Audited**
-   **Plutus Version:** Plutus V2
-   **Commit Hash:** [`9ec41e7619f5ba9d3dd46dd194e2146098093721`](https://github.com/fallen-icarus/cardano-swaps/commit/9ec41e7619f5ba9d3dd46dd194e2146098093721)
-   **Script Hashes:**
    - **One-Way:**
        -   Swap: `01fa36465dfe36e26c21fdbf720e4bdafcc0b86bb5367fca46012f56`
        -   Beacon Policy: `47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd`
    - **Two-Way:**
        -   Swap: `87381f0bf416e2dae7497d3fcd8087cf677b3cb4b2aeba36ed8f8f79`
        -   Beacon Policy: `84662c22dc5c0cadad7b2ebf9757ce9ea61dbd8fe64bc8c43c112a40`
-   **Audit Details:**
    -   **Auditor:** [Cypher Enterprises](https://github.com/cypher-enterprises)
    -   **Report:** [**View Full Report**](https://github.com/cypher-enterprises/p2p-audit/blob/main/audit.pdf)
