# Revision history for cardano-swaps

## 0.3.0.0

- Plutus scripts written using Aiken.
- `SwapDatum` changed to an sum (enum) type.
- Reference scripts are no longer required to be stored with beacon. Instead, a minimum UTxO value of 20 ADA is enforced. Users and arbitragers are expected to use there own or share using a program like [cardano-reference-scripts](https://github.com/fallen-icarus/cardano-reference-scripts). This is to cut down on unnecessary blockchain bloat.
- The `Close` redeemer no longer checks for outputs to the swap address since that is not what it is meant for.
- plutus-apps bumped to v1.2.0.
- Decoding datums from api query results now uses `decodeDatum` to minimize boilerplate.
- Koios support added.
- Swap contracts no longer convert prices from ADA to lovelace. All prices for ADA are assumed to be in units of lovelace. This cuts down on execution units used per execution.
- The CLI commands have changed. See the [GettingStarted](GettingStarted.md).

## 0.2.0.0

- All users get the same spending script for a given trading pair.
- There is a separate beacon policy for every trading pair.
- User addresses must have a staking credential.

## 0.1.0.0

* First version. Released on an unsuspecting world.
