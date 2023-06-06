# Revision history for cardano-swaps

## 0.3.0.0

- Plutus scripts written using Aiken.
- SwapDatum changed to an sum type.
- Reference script no longer required to be stored with beacon. Instead, a minimum UTxO value of 20 ADA is enforced.
- Closing no longer checks for outputs to swap address since that is not what it is meant for.
- plutus-apps bumped to v1.2.0.
- Decoding datums from api query results now uses `decodeDatum` to minimize boilerplate.
- Koios support added.

## 0.2.0.0

- All users get the same spending script for a given trading pair.
- There is a separate beacon policy for every trading pair.
- User addresses must have a staking credential.

## 0.1.0.0

* First version. Released on an unsuspecting world.
