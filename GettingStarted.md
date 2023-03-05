# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

When integration testing, it is highly recommended that you change the string passed to the mkBeaconPolicy function [here](src/CardanoSwaps.hs#L524). When developers make mistakes (myself included), it can create bad/locked utxos that will appear when you query the beacons. This can complicate your own testing. To avoid this, this extra parameter was added. Change the string to something unique to you. **Do this before building the executable in the installations section.** You should remember to change it to the desired string for mainnet.

---
## Table of Contents
- [Installing](#installing)
- [Minting test tokens](#minting-test-tokens)
- [Create a swap](#creating-a-swap)
- [Close a swap](#close-a-swap-using-a-reference-script)
- [Perform a swap](#perform-a-swap)
- [Update swap prices](#update-the-swap-prices)
- [Add to swap position](#add-to-swap-position)
- [Delegate the swap address](#delegate-the-swap-address)
- [Query available swaps](#query-available-swaps)
- [Staking PubKey Hash](#staking-pubkey-hash)

---
## Installing
Instructions are adapted from the [plutus-pioneers-program](https://github.com/input-output-hk/plutus-pioneer-program) week 1 exercise.

1. Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
2. Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, *stop* and fix the cache."

3. After adding the cache, you will need to restart the nix service. This can be done by executing `sudo systemctl restart nix` or by restarting your machine. If the cache was configured properly, you should see a lot of `copying path ... from 'https://cache.iog.io'` when you execute `nix-shell` in the next step.

4. Execute the following:
```
git clone https://github.com/fallen-icarus/cardano-swaps
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0
nix-shell           # this may take a while the first time

# Your terminal should now have a nix-shell prompt

cd ../cardano-swaps
cabal clean
cabal update
cabal build all
```
The `cardano-swaps` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-swaps-0.2.0.0/x/cardano-swaps/build/cardano-swaps/cardano-swaps`. Move the program to somewhere in your $PATH.

You can now exit the nix-shell with `exit`.

All `cardano-swaps` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

--- 
## Minting test tokens
An always succeeding minting policy as well as the required redeemer are included [here](scripts/mint-test-tokens/). In that directory is also the template bash script that uses them. These can be used to create as many native tokens as needed to test this DEX.

---
## Creating a Swap

### Export the spending script for that trading pair
``` Bash
cardano-swaps swaps export-script \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asked_policy_id> \
  --asked-asset-token-name <asked_token_name> \
  --out-file swap.plutus
``` 
To see all possible options, execute `cardano-swaps swaps --help`. The `offered-asset-is-ada` and `asked-asset-is-ada` options are there to make swapping ADA easier.

### Create the swap address with staking capabilities (using a staking pubkey)
``` Bash
cardano-cli address build \
  --payment-script-file swap.plutus \
  --stake-verification-key-file ownerStaking.vkey \
  --testnet-magic 1 \
  --out-file swap.addr
```

If you would like to use a staking script instead, use `--stake-script-file` with your staking script instead of `--stake-verification-key-file` with the staking pubkey.

### Export the beacon policy for that trading pair.
``` Bash
cardano-swaps beacons export-policy \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asked_policy_id> \
  --asked-asset-token-name <asked_token_name> \
  --out-file beacon.plutus
```

### Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

### Helper beacon variable
``` Bash
beacon="${beaconPolicyId}."
```

### Create the datum for storing with the beacon
``` Bash
cardano-swaps beacons create-datum \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asked_policy_id> \
  --asked-asset-token-name <asked_token_name> \
  --out-file beaconDatum.json
```

Even though the beacon is being stored in the swap address too, it needs a special datum.

### Create the datum for the first swap positions
``` Bash
cardano-swaps swaps create-datum \
  --swap-price 2 \
  --out-file swapDatum.json
```

### Create the beacon redeemer for minting the beacon.
``` Bash
cardano-swaps beacons create-redeemer \
  --mint-beacon \
  --out-file mint.json
```

### Build the transaction, sign it, and submit it
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_needs_to_pay_tx_fee> \
  --tx-out "$(cat swap.addr) + 23000000 lovelace + 1 <beacon_full_name>" \
  --tx-out-inline-datum-file beaconDatum.json \
  --tx-out-reference-script-file swap.plutus \
  --tx-out "$(cat swap.addr) + 15000000 lovelace" \
  --tx-out-inline-datum-file swapDatum.json \
  --mint "1 <beacon_full_name>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mint.json \
  --change-address $(cat owner.addr) \
  --tx-in-collateral <owner_needs_to_put_up_collateral> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Close a swap using a reference script
### Export the beacon policy for that trading pair.
``` Bash
cardano-swaps beacons export-policy \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asked_policy_id> \
  --asked-asset-token-name <asked_token_name> \
  --out-file beacon.plutus
```

### Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

### Helper beacon variable
``` Bash
beacon="${beaconPolicyId}."
```

### Create the close redeemer
``` Bash
cardano-swaps swaps create-redeemer \
  --close \
  --out-file close.json
```

### Create the beacon redeemer
``` Bash
cardano-swaps beacons create-redeemer \
  --burn-beacon \
  --out-file burn.json
```

### Create the transaction, sign it, and submit
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_swap_reference_script> \
  --spending-tx-in-reference <utxo_with_swap_reference_script> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file close.json \
  --tx-in <other_utxo_at_swap_address> \
  --spending-tx-in-reference <utxo_with_swap_reference_script> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file close.json \
  --mint "-1 <beacon_full_name>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file burn.json \
  --change-address $(cat owner.addr) \
  --required-signer-hash $(cat ownerStaking.pkh) \
  --tx-in-collateral <owner_needs_to_put_up_collateral> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --signing-key-file ownerStaking.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

This part should be repeated for every utxo at the swap address:
``` Bash
  --tx-in <other_utxo_at_swap_address> \
  --spending-tx-in-reference <utxo_with_swap_reference_script> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file close.json \
```

The `--required-signer-hash` option is required to tell the smart contract what key to look for. **This cannot make it so that a non-owner can close a swap**; it just needs to be present to properly build the transaction. The `ownerStaking.skey` must sign in addition to the usual payment key because the script will check if the address' staking credential approves of the transaction.

If the transaction is successfully built, then it is should to work on-chain as long as all tx-in utxos still exist when a stake pool operator goes to add your transaction and the transaction does not exceed the execution limits (if it does, no collateral will be lost).

---
## Perform a swap
### Create the Swap redeemer
``` Bash
cardano-swaps swaps create-redeemer \
  --swap \
  --out-file swap.json
```

### Create the swap datum for any change being returned to the swap address
Sometimes, specifying a decimal is not accurate enough for the DEX. Imagine a fraction like 54322819 / 1128891. A calculator may be forced to round the decimal version which will mean the price will defer from the one actually on-chain. For this reason, you can also create datums by directly saying what each utxo has.

``` Bash
cardano-swaps create-datum \
  --utxo-target-asset-balance 10 \
  --utxo-price-numerator 3 \
  --utxo-price-denominator 1 \
  --utxo-target-asset-balance 20 \
  --utxo-price-numerator 3 \
  --utxo-price-denominator 2 \
  --out-file price.json
```

This part should be repeated for each utxo being swapped:

``` Bash
  --utxo-target-asset-balance 20 \
  --utxo-price-numerator 3 \
  --utxo-price-denominator 2 \
```

`cardano-swaps` can properly create the datum from this. The `utxo-target-asset-balance` is the amount of the offered asset in that utxo. Any other assets included in the utxo can be ignored. This is just for calculating the weighted average price. The price calculated will be identical to the weighted price calculated by the script.

### Create the swap transaction, sign it, and submit
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <user_must_pay_transaction_fee> \
  --tx-in <first_desired_swap_utxo> \
  --spending-tx-in-reference <swap_reference_script_tx_ix> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file swap.json \
  --tx-in <second_desired_swap_utxo> \
  --spending-tx-in-reference <swap_reference_script_tx_ix> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file swap.json \
  --tx-out <change_to_swap_address> \
  --tx-out-inline-datum-file price.json \
  --tx-in-collateral <user_must_provide_collateral> \
  --change-address $(cat user.addr) \
  --protocol-params-file protocol.json \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file user.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

The user is responsible for properly giving the change back to each swap address. Make sure to remember that only the offered asset is allowed to leave each swap address. Therefore, if the offered asset is a native token, make sure to include the ADA the native token was stored with in the change to each swap address.

It is possible to create a swap transaction where nothing is actually removed from the swap address. This feature was added for being useful for gradually building up composed swaps when testing.

If the transaction successfully builds, then the swap should to work on-chain as long as the tx-in utxos still exist when it gets added to a block and the execution limits are not exceeded (if they are, no collateral will be lost). In the event that the utxos no longer exist, the transaction will fail without executing the scripts. This means the user's collateral is safe.

All of the information necessary for generating this transaction can be easily aquired with the `cardano-swaps query` subcommand (shown later).

---
## Update the swap prices
### Create the Update redeemer
``` Bash
cardano-swaps swaps create-redeemer \
  --update \
  --out-file update.json
```

### Create the datums for the newly created utxos
``` Bash
cardano-swaps swaps create-datum \
  --swap-price <desired_price_as_decimal> \
  --out-file price.json
```

### 3. Create the transaction, sign it, and submit
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_utxo_for_tx_fee> \
  --tx-in <non_reference_script_utxo_to_be_updated> \
  --spending-tx-in-reference <reference_for_swap_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file update.json \
  --tx-out <new_output_value_at_swap_address> \
  --tx-out-inline-datum-file price.json \
  --tx-in-collateral <owner_must_provide_collateral> \
  --change-address $(cat owner.addr) \
  --required-signer-hash $(cat ownerStaking.pkh) \
  --protocol-params-file protocol.json \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --signing-key-file ownerStaking.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

The `--required-signer-hash` is needed to successfully build the transaction. **This does not make non-owners capable of updating the asking price.** It is just necessary to successfully build the transaction.

*You do not need to update all utxos.* You can selectively update utxos as desired. You can also consolidate utxos if desired.

The utxo with the beacon cannot be updated. This is to minimize transaction fees. The beacon utxo can never be consumed in a swap so the datum attached to the beacon is never used. This makes it safe to ignore when updating prices.

If the transaction is successfully built, it should to work on-chain as long as the tx-in utxos still exist and execution limits are not exceeded.

---
## Add to swap position
New positions can always be added to swap addresses. Just output the desired utxo to the swap address and make sure to include the desired inline datum. There is a template bash script for this, too.

---
## Delegate the swap address
If the swap address is tied to a user's stake key, then all stake address related actions are identical to how normal user wallets are delegated.

### Create the registration certificate
``` Bash
cardano-cli stake-address registration-certificate \
  --stake-verification-key-file ownerStaking.vkey \
  --out-file registration.cert
```

### Create the delegation certificate
``` Bash
cardano-cli stake-address delegation-certificate \
  --stake-verification-key-file ownerStaking.vkey \
  --stake-pool-id <desired_pool_id> \
  --out-file delegation.cert
```

### Create the transaction, sign it, and submit
``` Bash
cardano-cli transaction build \
  --tx-in <owner_must_pay_fee> \
  --tx-in-collateral <owner_must_provide_collateral> \
  --change-address owner.addr \
  --certificate-file registration.cert \
  --certificate-file delegation.cert \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --signing-key-file ownerStaking.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Query available swaps
For now, `cardano-swaps` only supports the Blockfrost api. This is due to Koios not having a PreProduction Testnet api. You will need a Blockfrost ApiKey for this step. You can go [here](https://blockfrost.io/#pricing) to get one for free; only an email address is required.

To see how to use the command, execute `cardano-swaps query --help`. The results can either be saved to a file or displayed to stdout.

An example usage is below:
``` Bash
cardano-swaps query \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --stdout \
  --preprod-testnet $(cat api.txt)
```

When the above result is piped to `jq`, here is how it looks:

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

Only one utxo was found and that utxo only has lovelace in it. This output contains everything you need to remotely swap with it.

As of right now, the `cardano-swaps query` command will return the error of `"The requested component has not been found."` when that beacon has never been minted before. This is due to the beacon name being part of the Blockfrost api url like:

``` Url
https://cardano-preprod.blockfrost.io/api/v0/assets/{beacon_name}/addresses
```

A future version can address this.

---
## Staking PubKey Hash
To get the staking pubkey hash from a staking verification key, you can use this command:

``` Bash
cardano-cli stake-address key-hash \
  --stake-verification-key-file staking.vkey \
  --out-file staking.pkh
```
