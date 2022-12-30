# Getting Started

:warning: Assumes `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

All examples use the PreProduction Testnet.

---
## Table of Contents
- [Installing](#installing)
- [Minting test tokens](#minting-test-tokens)
- [Create a swap](#creating-a-swap)
- [Close a swap](#close-a-swap)
- [Perform a swap](#perform-a-swap)
- [Update swap prices](#update-the-swap-prices)
- [Add to swap position](#add-to-swap-position)
- [Delegate the swap address](#delegate-the-swap-address)
- [Get swap owner](#get-swap-owner)
- [Query available swaps](#query-available-swaps)

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
cabal build
```
The `cardano-swaps` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-swaps-0.1.0.0/x/cardano-swaps/build/cardano-swaps/cardano-swaps`. Move the program to somewhere in your $PATH.

You can now exit the nix-shell with `exit`.

All `cardano-swaps` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

--- 
## Minting test tokens
An always succeeding minting policy as well as the required redeemer are included [here](scripts/mint-test-tokens/). In that directory is also the template bash script that uses them. These can be used to create as many native tokens as needed to test this DEX.

---
## Creating a Swap
### 1. Get the owner's payment pubkey hash

``` Bash
cardano-cli address key-hash \
  --payment-verification-key-file owner.vkey \
  --out-file owner.pkh
```

### 2. Create a personal swap script
``` Bash
cardano-swaps swap-script create-script \
  --owner-payment-key-hash $(cat owner.pkh) \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asset_policy_name> \
  --asked-asset-token-name <asset_token_name> \
  --out-file swapScript.plutus
``` 
To see all possible options, execute `cardano-swaps swap-script create-script --help`. The `offered-asset-is-ada` and `asked-asset-is-ada` options are there to make swapping ADA easier.

### 3. Create staking script file (Optional)
``` Bash
cardano-swaps staking-script create-script \
  --owner-payment-key-hash $(cat owner.pkh) \
  --out-file stakingScript.plutus
```

To see all possible options, execute `cardano-swaps staking-script create-script --help`.

### 4. Create swap address with staking capabilities
``` Bash
cardano-cli address build \
  --payment-script-file swapScript.plutus \
  --stake-script-file stakingScript.plutus \
  --testnet-magic 1 \
  --out-file swap.addr
```

If you would like to use your personal staking key instead, use `--stake-verification-key-file` with your staking key instead of `--stake-script-file` with the staking script.

### 5. Create the stake address (if using a staking script)
``` Bash
cardano-cli stake-address build \
  --stake-script-file stakingScript.plutus \
  --testnet-magic 1 \
  --out-file staking.addr
```

### 6. Create the price datum
``` Bash
cardano-swaps swap-script create-datum \
  --swap-price 1.75 \
  --out-file price.json
```

### 7. Get the beacon policy id
``` Bash
cardano-swaps beacon policy-id --stdout
```

The beacon policy id can be saved to a file if `--out-file` is used instead of `--stdout`. The stdout option makes it easy to use in bash scripts.

### 8. Generate the beacon token name
``` Bash
cardano-swaps beacon generate-token-name \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asset_policy_name> \
  --asked-asset-token-name <asset_token_name> \
  --stdout
```

The beacon token name can be saved to a file if `--out-file` is used instead of `--stdout`.

### 9. Create the full beacon name
``` Haskell
beacon = policyId ++ "." ++ tokenName
```

### 10. Get the beacon vault address
``` Bash
cardano-swaps beacon vault-script --out-file beaconVault.plutus

cardano-cli address build \
  --payment-script-file beaconVault.plutus \
  --testnet-magic 1 \
  --out-file beaconVault.addr
```

The beacon vault address will be the same for everyone. However, since it is different for each network id it is not hardcoded into `cardano-swaps` so it will need to be re-derived each time.

### 11. Create the datum for the beacon vault
``` Bash
cardano-swaps beacon create-datum --out-file beaconVaultDatum.json
```

### 12. Create the beacon redeemer
``` Bash
cardano-swaps beacon create-redeemer \
  --mint-beacon <beacon_token_name> \
  --out-file beaconRedeemer.json
```

### 13. Get the beacon policy script
``` Bash
cardano-swaps beacon policy-script --out-file beacon.plutus
```

### 14. Build the transaction, sign it, and submit it
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_needs_to_pay_tx_fee> \
  --tx-out "$(cat swap.addr) + 24000000 lovelace + 1 <beacon_full_name>" \
  --tx-out-inline-datum-file price.json \
  --tx-out-reference-script-file swap.plutus \
  --tx-out "$(cat beaconVault.addr) + 2000000 lovelace" \
  --tx-out-inline-datum-file beaconVaultDatum.json \
  --tx-out "$(cat swap.addr) + 150000000 lovelace" \
  --tx-out-inline-datum-file price.json \
  --mint "1 <beacon_full_name>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file beaconRedeemer.json \
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
```

The first two tx-outs will be the same for every swap. 24 ADA is required to store the reference script on-chain. This can be recovered upon closing the swap as long as the utxo is given a price datum.

If this transaction successfully builds, then the transaction is guaranteed to succeed on-chain as long as the tx-in utxos still exist.

You can submit using any method you like (cardano-node,Blockfrost,Koios,etc.).

---
## Close a swap
### 1. Create close redeemer
``` Bash
cardano-swaps swap-script create-redeemer \
  --close-swap \
  --out-file close.json
```

### 2. Get the beacon policy id
``` Bash
cardano-swaps beacon policy-id --stdout
```

### 3. Generate the beacon token name
``` Bash
cardano-swaps beacon generate-token-name \
  --offered-asset-is-ada \
  --asked-asset-policy-id <asset_policy_name> \
  --asked-asset-token-name <asset_token_name> \
  --stdout
```

### 4. Create the full beacon name
``` Haskell
beacon = policyId ++ "." ++ tokenName
```

### 5. Get the beacon policy script
``` Bash
cardano-swaps beacon policy-script --out-file beacon.plutus
```

### 6. Get the beacon vault script
``` Bash
cardano-swaps beacon vault-script --out-file beaconVault.plutus
```

### 7. Create beacon redeemer
``` Bash
cardano-swaps beacon create-redeemer \
  --burn-beacon <beacon_token_name> \
  --out-file beaconRedeemer.json
```

### 8. Create the transaction, sign it, and submit
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_needs_to_pay_tx_fee> \
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
  --tx-in <deposit_utxo_at_beacon_vault> \
  --tx-in-script-file beaconVault.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file beaconRedeemer.json \
  --mint "-1 <beacon_full_name>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file beaconRedeemer.json \
  --change-address $(cat owner.addr) \
  --required-signer-hash $(cat owner.pkh) \
  --tx-in-collateral <owner_needs_to_put_up_collateral> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --testnet-magic 1 \
  --out-file tx.signed
```

This part should be repeated for every utxo at the swap address:
``` Bash
  --tx-in <other_utxo_at_swap_address> \
  --spending-tx-in-reference <utxo_with_swap_reference_script> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file close.json \
```

The `--required-signer-hash` option is required to tell the smart contract what key to look for. **This cannot make it so that a non-owner can close a swap**; it just needs to be present to properly build the transaction.

If the transaction is successfully built, then it is guaranteed to work on-chain as long as all tx-in utxos still exist when a stake pool operator goes to add your transaction. Submit the transaction however you like.

---
## Perform a swap
### 1. Create the Swap redeemer
``` Bash
cardano-swaps swap-script create-redeemer \
  --swap-assets \
  --out-file swap.json
```

### 2. Create the swap datum for any change being returned to the swap address
This part will depend on how many utxos you are looking to use from a swap address. If there is only one utxo being consumed, the following can suffice:

``` Bash
cardano-swaps swap-script create-datum \
  --swap-price <decimal_equivalent_of_price_fraction> \
  --out-file price.json
```

However, this method may not work for all cases. Imagine a fraction like 54322819 / 1128891. A calculator may be forced to round the decimal version which will mean the price datum will defer from the one actually on-chain.

The other method you can use in this case, and in the case where there are multiple utxos being consumed from a swap address, is to create a JSON file with the necessary information. `cardano-swaps` can properly create the price datum from this file. First, export the JSON template:

``` Bash
cardano-swaps swap-script create-datum \
  --swap-price-file-template \
  --out-file template.json
```

The JSON file will look like this:

``` JSON
[
    {
        "priceDenominator": 1,
        "priceNumerator": 1,
        "utxoAmount": 100
    },
    {
        "priceDenominator": 1,
        "priceNumerator": 2,
        "utxoAmount": 200
    }
]
```

The `utxoAmount` is the amount of the offered asset in the swap. Any other assets included in the utxo can be ignored. This is just for calculating the weighted average asking price. 

Properly edit the template for the necessary number of utxos. You can then create the correct datum using this file:

``` Bash
cardano-swaps swap-script create-datum \
  --calc-swap-price-from-file template.json \
  --out-file price.json
```

The price calculated will be identical to the weighted price calculated by the script.

### 3. Create the swap transaction, sign it, and submit
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
  --tx-out "<user_address> 2000000 lovelace + 300 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "<swap_address> + 4000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 250 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
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
```

You are responsible for properly giving the change back to the swap address. Make sure to remember that only the offered asset is allowed to leave the swap address. Therefore, if the offered asset is a native token, make sure to include the ADA the native token was stored with in the change to the swap address.

There is no requirement that swap utxos must come from the same swap address. You can "chain" swaps together by including utxos from different swap addresses; just make sure to keep track of which reference script is for which utxo. When you "chain" swaps, you will need to account for the change to ALL the swap addresses used.

It is possible to create a swap transaction where nothing is actually removed from the swap address. This feature was added for being useful for gradually building up chain swaps when testing.

If the transaction successfully builds, then the swap is guaranteed to work on-chain as long as the tx-in utxos still exist when it gets added to a block. In the event that the utxos no longer exist, the transaction will fail without executing the scripts. This means the user's collateral is safe.

All of the information necessary for generating this transaction can be easily aquired with the `cardano-swaps query-swaps` subcommand (shown later).

Submit the transaction however you like.

---
## Update the swap prices
### 1. Create the Update redeemer
``` Bash
cardano-swaps swap-script create-redeemer \
  --update-swap-price <desired_price_as_decimal> \
  --out-file update.json
```

### 2. Create the datums for the newly created utxos
``` Bash
cardano-swaps swap-script create-datum \
  --swap-price <desired_price_as_decimal> \
  --out-file price.json
```

### 3. Create the transaction, sign it, and submit
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_must_pay_tx_fee> \
  --tx-in <non_reference_script_utxo_to_be_updated> \
  --spending-tx-in-reference 294071a64a9ddb540de29880747f148f2b857a7908049f68864162b39c42aaf2#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file update.json \
  --tx-out "<swap_address> + 2000000 lovelace + 1000 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a" \
  --tx-out-inline-datum-file price.json \
  --tx-in-collateral <owner_must_provide_collateral> \
  --change-address $(cat owner.addr) \
  --required-signer-hash $(cat owner.pkh) \
  --protocol-params-file protocol.json \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --testnet-magic 1 \
  --out-file tx.signed
```

The `--required-signer-hash` is needed to successfully build the transaction. **This does not make non-owners capable of updating the asking price.** It is just necessary to successfully build the transaction.

The reason the `Update` redeemer takes a price argument is to improve script execution efficiency. It is used to make sure the datums are of the proper price.

*You do not need to update all utxos.* You can selectively update utxos as desired. You can also consolidate utxos if desired.

The utxo with the reference script cannot be updated. This is to minimize transaction fees. The reference script can never be consumed in a swap so the datum attached to the reference script is never used. This makes it safe to ignore when updating prices.

If the transaction is successfully built, it is guaranteed to work on-chain as long as the tx-in utxos still exist.

Submit the transaction however you like.

---
## Add to swap position
New positions can always be added to swap addresses. Just output the desired utxo to the swap address and make sure to include the desired price inline datum. There is a template bash script for this, too.

---
## Delegate the swap address
If the swap address is tied to a user's stake key, then all stake address related actions are identical to how normal user wallets are delegated.

If the swap address is tied to the swap script instead, then delegating is slightly different.

### 1. Create the staking redeemer
``` Bash
cardano-swaps staking-script create-redeemer --out-file stake.json
```

### 2. Create the registration certificate
``` Bash
cardano-cli stake-address registration-certificate \
  --stake-script-file staking.plutus \
  --out-file registration.cert
```

### 3. Create the delegation certificate
``` Bash
cardano-cli stake-address delegation-certificate \
  --stake-script-file staking.plutus \
  --stake-pool-id <desired_pool_id> \
  --out-file delegation.cert
```

### 4. Create the transaction, sign it, and submit
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <owner_must_pay_fee> \
  --tx-in-collateral <owner_must_provide_collateral> \
  --change-address owner.addr \
  --certificate-file registration.cert \
  --certificate-file delegation.cert \
  --certificate-script-file staking.plutus \
  --certificate-redeemer-file stake.json \
  --required-signer-hash $(cat owner.pkh) \
  --protocol-params-file protocol.json \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file owner.skey \
  --testnet-magic 1 \
  --out-file tx.signed
```
Redelegating and deregistering are done similarly.

**When the staking script is used, staking actions require the owner's payment skey signature, not the owner's staking skey.**

If the transaction successfully builds, then the transaction is guaranteed to work on-chain. 

---
## Get swap owner
### 1. Create the Info redeemer
``` Bash
cardano-swaps swap-script create-redeemer \
  --owner-info \
  --out-file info.json
```

### 2. Build the transaction; there is no need to sign and submit it
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <any_utxo_from_swap_address> \
  --spending-tx-in-reference <swap_reference_script_tx_ix> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file info.json \
  --tx-in-collateral <user_must_provide_collateral> \
  --change-address $(cat user.addr) \
  --protocol-params-file protocol.json \
  --testnet-magic 1 \
  --out-file tx.body
```

Building this transaction is guaranteed to fail. In order to see the owner information, it must look legitamite which is why the user must still provide the collateral. The collateral is safe since this transaction will not actually be submitted.

Here is what the output looks like:

``` Txt
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in the order of the TxIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Caused by: [
  (builtin decodeUtf8)
  (con bytestring #fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f)
]
Script debugging logs: 
```

`fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f` is the owner's payment pubkey hash.

---
## Query available swaps
For now, `cardano-swaps` only supports the Blockfrost api. This is due to Koios not having a PreProduction Testnet api. You will need a Blockfrost ApiKey for this step. You can go [here](https://blockfrost.io/#pricing) to get one for free; all that is required is an email address.

To see how to use the command, execute `cardano-swaps query-swaps --help`. The results can either be saved to a file or displayed to stdout.

An example usage is below:
``` Bash
cardano-swaps query-swaps \
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

As of right now, the `cardano-swaps query-swap` command will return the error of `"The requested component has not been found."` when there are no swap addresses with that beacon. This is due to the beacon name being part of the Blockfrost api url like:

``` Url
https://cardano-preprod.blockfrost.io/api/v0/assets/{beacon_name}/addresses
```

A future version can address this.
