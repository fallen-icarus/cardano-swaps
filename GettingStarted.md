# Getting Started

`cardano-swaps` assumes that all transactions are built and signed using `cardano-cli`. **Access to
a local node is not necessary**, although it does simplify things. Koios can be used for all steps
that require access to a node.

Template bash scripts that follow these steps are available [here](scripts/). There are examples
using a local node and a remote node. Using a remote node requires extra steps since the transaction
must be balanced manually.

## Table of Contents
- [Installing](#installing)
- [Aiken For Developers](#aiken-for-developers)
- [`cardano-cli` Auto-Balancing](#cardano-cli-auto-balancing)
- [Using Remote Nodes](#using-remote-nodes)
- [Minting Test Tokens](#minting-test-tokens)
- [One-Way Swaps](#one-way-swaps)
  - [Creating Reference Scripts](#creating-reference-scripts)
  - [Creating A Swap](#creating-a-swap)
  - [Closing A Swap](#closing-a-swap)
  - [Updating A Swap](#updating-a-swap)
  - [Converting A Swap To A New Trading Pair](#converting-a-swap-to-a-new-trading-pair)
  - [Executing A Swap](#executing-a-swap)



## Installing

Make sure `cardano-cli` is also installed. You can get the most up-to-date copy from IOG's
cardano-node repo [here](https://github.com/input-output-hk/cardano-node). It will be in the
cardano-node tarball under the latest release.

### Install the necessary packages - similar to cardano-node
```
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

### Install libsodium and scep256k1
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

### Install GHC 8.10.7 and cabal
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

### Build the executable
```
git clone https://github.com/fallen-icarus/cardano-swaps
cd cardano-swaps
cabal clean
cabal update
cabal build exe:cardano-swaps
```

The `cardano-swaps` CLI program should now be at
`dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-swaps-1.0.0.0/x/cardano-swaps/build/cardano-swaps/cardano-swaps`.
Move the program to somewhere in your `$PATH`.

All `cardano-swaps` subcommands have an associated `--help` option. The functionality is meant to
feel like `cardano-cli`.

The smart contracts are compiled *into* the created `cardano-swaps` CLI. The executable has
everything you need for using the DEX.

## Aiken For Developers

The aiken smart contracts come precompiled but if you would like to make changes or wish to confirm
the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using
cargo like this:

``` Bash
cargo install aiken --version 1.0.20-alpha
```

Make sure you instal verison 1.0.20-alpha. Newer versions may change some things and so the source
code may not compile or may result in a different script. As aiken stabilizes, the code will be
updated to the latest version.

When building the dApp's blueprints, make sure to use

``` Bash
aiken build --keep-traces
```

or else the user friendly error messages will be stripped from the smart contracts and the resulting
beacons will be different.

For integration testing, you can create your own custom beacons without changing the dApp's logic by
changing the string passed [here](aiken/lib/cardano_swaps/common/types.ak#L4). Currently, it is set
to "". You can change this to any string personal to you so that you can get custom beacons
to play with for testing.

If you would like to use the `cardano-swaps` CLI after making your changes, you will need to
rebuild it with `cabal bulid exe:cardano-swaps`. As long as you did not make any breaking changes,
the CLI should still work for you.

If you would like to test your changes, you can run the tests using `cabal run tests`. As long
as you did not make any breaking changes, the tests should quickly give you feedback. There are
four kinds of tests:

1) Regression tests - tests for features that should work.
2) Failure tests - tests for scenarios that are supposed to fail.
3) Bench tests - tests to check for degraded performance in specific scenarios.
4) Performance Increase tests - tests to check for improved performance in specific scenarios.

To see the documentation for the tests, you can build the haddocks for the tests using `cabal
haddock tests`. The documentation may be easier to read than the source code. You can view the
documentation in any browser.

## `cardano-cli` Auto-Balancing

While `cardano-cli` is able to auto-balance transactions, the auto-balancer does not work when
scripts are executed in a transaction where native tokens must go to the change address. It does not
properly add the change *before* estimating the execution budgets for the transaction which always
results in it under-estimating the required execution units needed by the scripts. There are open
issues about this [here](https://github.com/input-output-hk/cardano-node/issues/5386) and
[here](https://github.com/input-output-hk/cardano-api/issues/302). If you ever see a very long and
confusing error message about overspending budgets, this is probably the issue.

As a work around, whenever you build a transaction using `cardano-cli transaction build` where
scripts are being executed, you must manually create an output that has all of the native tokens
that would normally go into the change output. You can let the auto-balancer balance the ADA.

## Using Remote Nodes

`cardano-cli transaction build` requires a local node for the auto-balancer which means it cannot be
used to build a transaction. Instead, the `cardano-cli transaction build-raw` command is required.
This command requires three steps:
1. Build a temporary transaction that is missing the execution units and transaciton fee but is
   properly balanced. You can assume a fee of zero for this transaction.
2. Submit the temporary transaction for execution budget estimations.
3. Rebuild the transaction with the proper execution budgets. The fee is still set to zero.
4. Calculate the required fee for this new temporary transaction.
5. Create the final transaction with the required fee and properly balanced outputs (subtract off
   the fee from the change).
6. Sign the transaction and submit to a remote node.

Submitting a transaction for execution budget estimations can be done with this command:
```Bash
cardano-swaps evaluate-tx \
  --testnet \
  --tx-file tx.body
```

The returned budgets will be indexed by the input order. **This may not be the same order you
specified when building the temporary transaction.** The node will reorder the inputs
lexicographically based on the inputs' tx hashes and output indexes.

Submitting the final transaction for addition to the blockchain can be done with this command:
```Bash
cardano-swaps submit \
  --testnet \
  --tx-file tx.signed
```

## Minting Test Tokens

An always succeeding minting policy as well as the required redeemer are included with template bash
scripts for either a local node or a remote node. These can be used to create as many native tokens
as needed to test this DEX.

To see how to mint test tokens using a local node, refer 
[here](scripts/local/mint-test-tokens/mint.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/mint-test-tokens/mint.sh).


## One-Way Swaps

### Creating Reference Scripts

Creating reference scripts involves the following steps:
1. Export the scripts from the `cardano-swaps` CLI.
2. Submit a transaction with the reference scripts stored in the outputs.

##### Exporting the scripts
```Bash
# Export the swap validator script.
cardano-swaps scripts one-way swap-script \
  --out-file oneWaySwap.plutus

# Export the beacon policy.
cardano-swaps scripts one-way beacon-policy \
  --out-file oneWayBeacons.plutus
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/create-reference-scripts.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/create-reference-scripts.sh).

### Creating A Swap

Creating a swap involves the following steps:
1. Create your swap address.
2. Calculate the required beacon names to mint.
3. Create the required beacon minting redeemer.
4. Create the required swap datum for each swap.
5. Submit a transaction that creates the swaps.

##### Creating your swap address
```Bash
# Export the swap validator script.
cardano-swaps scripts one-way swap-script \
  --out-file oneWaySwap.plutus

# Create the swap address.
cardano-cli address build \
  --payment-script-file oneWaySwap.plutus \
  --stake-verification-key-file ownerStake.vkey \
  --testnet-magic 1 \
  --out-file oneWaySwap.addr
```

Cardano-Swaps also supports using staking scripts for the address. To use a staking script, use
the `--stake-script-file` flag instead of the `--stake-verification-key-file` flag.

For a mainnet address, just use the `--mainnet` flag instead of `--testnet-magic 1` when creating
the address.

##### Calculate the required beacon names to mint
One-way swaps require two beacons: the trading pair beacon and the offer beacon.

```Bash
# Get the policy id for the one-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

# Get the required offer beacon name.
offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

# Create the required full beacon names.
pairBeacon="${beaconPolicyId}.${pairBeaconName1}"
offerBeacon="${beaconPolicyId}.${offerBeaconName1}"
```

The above beacons are for a swap that is offering a native token in exchange for ADA.

##### Create the required minting redeemer
```Bash
cardano-swaps beacon-redeemers one-way \
  --create-swap \
  --out-file createOneWaySwap.json
```

##### Creating the required swap datum
```Bash
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --price-numerator 1000000 \
  --price-denominator 1 \
  --out-file oneWaySwapDatum.json
```

**The price is always Ask/Offer.** In the above example, the swap wants 1 ADA per 1 native token
taken.

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/create-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/create-swap.sh).


### Closing A Swap

Closing a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending redeemer.
3. Calculate the required beacon names to burn.
4. Create the required beacon burning redeemer.
5. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```Bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending redeemer
```Bash
cardano-swaps spending-redeemers one-way \
  --close-or-update \
  --out-file closeOrUpdateOneWaySwap.json
```

##### Calculate the required beacon names to burn
```Bash
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName1}"
offerBeacon="${beaconPolicyId}.${offerBeaconName1}"
```

##### Create the required burning redeemer
```Bash
cardano-swaps beacon-redeemers one-way \
  --burn \
  --out-file burnOneWayBeacons.json
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/close-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/close-swap.sh).


### Updating A Swap

Updating a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending redeemer.
3. Create the new swap datum.
4. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```Bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending redeemer
```Bash
cardano-swaps spending-redeemers one-way \
  --close-or-update \
  --out-file closeOrUpdateOneWaySwap.json
```

##### Creating the new swap datum
```Bash
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --price-numerator 1000000 \
  --price-denominator 2 \
  --out-file oneWaySwapDatum.json
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/update-price.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/update-price.sh).


### Converting A Swap To A New Trading Pair

Converting a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending redeemer.
3. Create the required minting redeemer.
4. Calculate the required beacon names to mint. These are for the new pair.
5. Calculate the required beacon names to burn. These are for the old pair.
6. Create the new swap datum.
7. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```Bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending redeemer
```Bash
cardano-swaps spending-redeemers one-way \
  --close-or-update \
  --out-file closeOrUpdateOneWaySwap.json
```

##### Create the required minting redeemer
```Bash
cardano-swaps beacon-redeemers one-way \
  --create-swap \
  --out-file createOneWaySwap.json
```

This redeemer can be used to both burn the old beacons and mint the new ones.

##### Calculate the required beacon names to mint
One-way swaps require two beacons: the trading pair beacon and the offer beacon.

```Bash
# Get the policy id for the one-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
newPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

# Get the required offer beacon name.
newOfferBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

# Create the required full beacon names.
newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newOfferBeacon="${beaconPolicyId}.${newOfferBeaconName}"
```

The above beacons are for a swap that is offering a native token in exchange for ADA.

##### Calculate the required beacon names to burn

```Bash
# Get the policy id for the one-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
oldPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

# Get the required offer beacon name.
oldOfferBeacon=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

# Create the required full beacon names.
oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldOfferBeacon="${beaconPolicyId}.${oldOfferBeacon}"
```

##### Creating the new swap datum
```Bash
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --price-numerator 1000000 \
  --price-denominator 2 \
  --out-file oneWaySwapDatum.json
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/convert-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/convert-swap.sh).



### Executing A Swap

Executing a swap involves the following steps:
1. Create the spending redeemer.
2. Create the corresponding swap datum for the target swap.
3. Submit the transaction.

It may be helpful to also calculate the beacon names and store them as variables since the swap
output still needs them.

##### Create the spending redeemer
```Bash
cardano-swaps spending-redeemers one-way \
  --swap \
  --out-file $swapRedeemerFile
```

##### Create the corresponding swap datum for the target swap.
```Bash
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --price-numerator 1 \
  --price-denominator 50000 \
  --tx-hash 71ae3f66eead7198a79232ff8f2c032d845d0070d3f066f1b5dec3c2abe99788 \
  --output-index 0 \
  --out-file $swapDatumFile1
```

The datum should be *exactly* the same as the target swap's datum except the new datum should point
to the swap input: the `--tx-hash` and `--output-index` flags should specify the UTxO of the swap
being consumed.


##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/swap-assets.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/swap-assets.sh).
