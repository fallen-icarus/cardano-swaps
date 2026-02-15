# Revision history for cardano-swaps off-chain code

> For the on-chain smart contract protocol specification, see [VERSIONS.md](VERSIONS.md).
> For development history prior to Protocol v1, see [CHANGELOG-archive.md](CHANGELOG-archive.md).

## 2.0.0

- Updated off-chain CLI to use the new Protocol v2 contracts.
- Updated `src/` library to use the new Protocol v2 contracts.
- Updated smart contract unit tests for the new contracts.

## 1.0.0

- Updated off-chain code to allow building with ghc-9.x. The new default ghc version is set to
9.6.4.
- Refactored the CardanoSwaps haskell library to make it easier to maintain separate versions
going forward.
- Updated off-chain CLI to lookup the current protocol parameters from Koios instead of them
being hard-coded into the executable.
- Added GettingStarted instructions for building required aiken compiler from source. **The same
compiler version must be used by everyone.**
- Added two more failure test cases to the smart contract unit tests.
- Simplified and updated example scripts for conway era.
- Added missing `--tx-out-return-collateral` fields to example remote scripts.
