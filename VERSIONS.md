# Cardano-Swaps Protocol Versions

This document provides the official specification for each version of the Cardano-Swaps smart
contract protocol.

A new protocol version (e.g., v1, v2) is released **only when the on-chain smart contract code is
changed**. Each version corresponds to a unique, immutable set of smart contracts.

---

## Protocol v2 - *In Development*

This version introduces the optional order `expiration` feature to both one-way and two-way swaps.

> [!NOTE]
> A `RegisterScript` redeemer was added for the beacon scripts since the conway hardfork now
> enforces script execution when registering a staking credential.

-   **Status:** 🟡 **Planned**
-   **Plutus Version:** Plutus V2
-   **Commit Hash:** [`046456b84eca2c830e0be1e7e33cc73f49f62388`](https://github.com/fallen-icarus/cardano-swaps/commit/046456b84eca2c830e0be1e7e33cc73f49f62388)
-   **Script Hashes:**
    - **One-Way:**
        -   Swap: `1d6cff26bcab91d2061aad0bd259cbb7d76d25ced2eeaed5926a42ad`
        -   Beacon Policy: `c4d7d117d9ebcde6db28db40837ff2b1401e9eaaa6eecea9e070e209`
    - **Two-Way:**
        -   Swap: `11928a3ac3b65edbf103ea6bb3362e39b879a36f02897df31c40917b`
        -   Beacon Policy: `8a199a17ef4517215945aaf3c8c5204c60fd94d34c46d341e99c8fcf`

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
