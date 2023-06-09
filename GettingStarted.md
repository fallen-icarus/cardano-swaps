# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

For now, Cardano-Swaps is only for the testnet. It seems negligent to release it on mainnet before:
1. It has undergone a security audit.
2. Aiken has an official mainnet release.

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
### Using Cabal - RECOMMENDED

#### Install the necessary packages - similar to cardano-node
```
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

#### Install libsodium and scep256k1
```
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig
```

Add the following lines to your `$HOME/.bashrc` file:
```
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

#### Install GHC 8.10.7 and cabal
```
cd
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
Make sure to install the required packages it mentions before hitting ENTER.

Prepend or append the required PATH variable.

You do not need to install the haskell-langauge-server.

You do not need to install stack.

Press ENTER to proceed.
```
source .bashrc
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7
```

#### Build the executable
```
git clone https://github.com/fallen-icarus/cardano-swaps
cd cardano-swaps
cabal clean
cabal update
cabal build all
```

The `cardano-swaps` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-swaps-0.3.0.0/x/cardano-swaps/build/cardano-swaps/cardano-swaps`. Move the program to somewhere in your `$PATH`.

All `cardano-swaps` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

### Using Nix
The [Nix Package Manager](https://nixos.org/) can be installed on most Linux distributions by downloading and running the installation script
```
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```
and following the directions.

#### Configuring the Binary Caches
While this step is optional, it can save several hours of time since nix will need a copy of every necessary package. Therefore, it is highly recommended that you do this.
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
experimental-features = nix-command flakes
allow-import-from-derivation = true
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
EOF
```
The caches used here come from the plutus-apps contributing [doc](https://github.com/input-output-hk/plutus-apps/blob/713955dea45739de6df3c388717123cfec648914/CONTRIBUTING.adoc#how-to-get-a-shell-environment-with-tools).

You will need to restart the nix service in order to make sure that it uses the newly configured caches. A sure fire way to do this is restart your machine.

#### Building the Executable
```
git clone https://github.com/fallen-icarus/cardano-swaps
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout 68c3721
nix develop # This step can take an hour even with the caches configured
# Set accept-flake-config to true and permanently mark the value as trusted
```
The last command should drop you into a nix terminal once it is finished running. Execute the following within the nix terminal.
```
cd ../cardano-swaps
cabal clean
cabal update
cabal build all
```

If all goes well, the `cardano-swaps` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-swaps-0.3.0.0/x/cardano-swaps/build/cardano-swaps/cardano-swaps`. Move the program to somewhere in your $PATH.

You can now exit the nix terminal with `exit`.

All `cardano-swaps` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

#### Troubleshooting Nix
If you encounter a libsodium error, you may need to first install libsodium separately. While not inside the nix terminal (you can leave with `exit`), execute the following:
```
cd # return to your home directory
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install
```
Once installed, you can retry the build after exporting the following variables while inside the nix terminal:
```
cd ../plutus-apps
nix develop # This should only take a minute this time
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
cabal build all
```

### Aiken For Developers
The aiken scripts come precompiled but if you would like to make changes or wish to confirm the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using cargo like this:

``` Bash
cargo install aiken --version 1.0.8-alpha
```

When building the dApp's blueprints, make sure to use
``` Bash
aiken build --keep-traces
```
or else the user friendly error messages will be stripped from the scripts and the resulting beacons will be different.

For integration testing, you can create your own custom beacons without changing the dApp's logic by changing the string passed [here](aiken/validators/cardano_swaps.ak#L13). Currently, it is set to "testing". You can change this to any string personal to you so that you can get custom beacons to play with for testing.

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
