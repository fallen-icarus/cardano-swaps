# Cardano-Swaps Protocol Versions

This document provides the official specification for each version of the Cardano-Swaps smart
contract protocol.

A new protocol version (e.g., v1, v2) is released **only when the on-chain smart contract code is
changed**. Each version corresponds to a unique, immutable set of smart contracts.

---

## Protocol v2 - *Current*

This version makes the following changes to both one-way and two-way swaps:

- Introduces the optional order `expiration` feature.
- Beacon names are now derived from the `sha2_256` hash of the CBOR-serialised asset/pair
  constructors, making the naming scheme injective.
- The staking credential of a new swap's address must approve the creating transaction, so swaps
  cannot be accidentally created at an address whose staking credential is unusable. The check
  only applies when the new swap's `prev_input` is `None`. A swap created with `Some` (e.g. to
  link an update to the order it replaces) skips it, so the owner must verify the credential
  themselves (audit finding CSW-302).

-   **Status:** ✅ **Live & Audited**
-   **Plutus Version:** Plutus V3
-   **Commit Hash:** [`e1ab915709def43692dcde3a69fcfa23313a9bcb`](https://github.com/fallen-icarus/cardano-swaps/commit/e1ab915709def43692dcde3a69fcfa23313a9bcb)
-   **Script Hashes:**
    - **One-Way:**
        -   Swap: `e5a22e4c31db20bce1c8b081f8e4009683990a33157947d75030deb8`
        -   Beacon Policy: `274765b4c626c28d18752176b59c0ff63db56b8305c1daa49c9879fe`
    - **Two-Way:**
        -   Swap: `569431d7c48bebb283077d46cbc72bd1f9f57f94c3b6d700aa684095`
        -   Beacon Policy: `ae19cf56a7631068aa754327e0472840b76e485868a324904c8b379e`
-   **Audit Details:**
    -   **Auditor:** [TxPipe](https://txpipe.io)
    -   **Report:** [**View Full Report**](./audits/v2/2026-09-21-txpipe.pdf)

---

## Protocol v1 - *Legacy*

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
    -   **Report:** [**View Full Report**](./audits/v1/2025-08-22-cypher-enterprises.pdf) ([original](https://github.com/cypher-enterprises/p2p-audit/blob/main/audit.pdf))
