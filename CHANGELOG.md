# Revision history for cardano-swaps

## 2.0.0.0rc

- Bumped aiken stdlib version to 1.7.0.
- Added staking execution option to the beacon scripts.
- Changed the spending scripts to delegate to the beacon scripts for owner related actions.
- Split the owner spending redeemers into separate redeemers based on whether the beacon script
will use a staking execution or a minting execution.
- Updated the CLI to use the new redeemers.
- Updated the template scripts to show how to use the new redeemers and staking execution.
- Update the tests and benchmarks.
- Updated the documentation.
- Changed the internal representation of prices from `Rational` to Int. Performance boost.
- Changed how datums were checked during swap executions. Performance boost.
- Changed how values are checked. Major performance boost.

## 1.0.0.0rc

- Added two-way swaps to provide a mechanism to naturally incentivize liquidity providers.
- Changed one-way swaps to use a single beacon policy for all swaps. The asset name now
distinguishes the beacon.
- Changed one-way swaps to not support merging swap outputs. This dramatically improved performance.
- Support for Blockfrost was dropped in the CLI because it does not allow server-side filtering of
swaps like Koios does. For example, querying swaps by trading pair also needs to filter out all
finished swaps. Koios can do this *before* returning the results. Blockfrost requires this filtering
to be done locally.
- Added CLI support for using Koios as a remote node.
- Added template bash scripts for using a remote node.
- Added CLI support for querying personal addresses using Koios.

## 0.4.0.0

- Universal spending script used instead of one spending script for each trading pair.
- One beacon policy for each asset being offered. The asset name is: sha2_256( ask_asset policy id ++ ask_asset asset name ). This allows for offer based queries in addition to trading pair based queries.
- The minimum deposit of 20 ADA was removed. Instead the proper beacon must be stored with each swap UTxO. This requires the minUTxO value due to the protocol parameters.
- The swap datum was expanded to include information about the swap UTxO: beacon policy id and name, offer asset policy id and name, ask asset policy id and name, and price.
- Added check for the beacon to be stored with some of the offer asset during creating and updating swaps.
- Added check for price to have a denominator > 0.
- Added check that no extraneous assets can be stored in the swap UTxO.
- Merged Close and Update redeemer into one redeemer.

## 0.3.0.0

- Plutus scripts written using Aiken.
- `SwapDatum` changed to a sum (enum) type.
- Reference scripts are no longer required to be stored with the beacons. Instead, a minimum UTxO value of 20 ADA is enforced. Users and arbitragers are expected to use there own reference scripts or share using a program like [cardano-reference-scripts](https://github.com/fallen-icarus/cardano-reference-scripts). This is to cut down on unnecessary blockchain bloat due to multiple copies of the same script being stored on chain.
- The `Close` redeemer no longer checks for outputs to the swap address since that is not what it is meant for.
- plutus-apps bumped to v1.2.0.
- Decoding datums from api query results now uses `decodeDatum` to minimize boilerplate.
- Koios support added.
- Swap contracts no longer convert prices from ADA to lovelace. All prices for ADA are assumed to be in units of lovelace. This cuts down on cost used per execution.
- The CLI commands have changed. See the [GettingStarted](GettingStarted.md).

## 0.2.0.0

- All users get the same spending script for a given trading pair.
- There is a separate beacon policy for every trading pair.
- User addresses must have a staking credential.

## 0.1.0.0

* First version. Released on an unsuspecting world.
