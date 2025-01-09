# Getting Started

The `cardano-swaps` CLI assumes that all transactions are built and signed using `cardano-cli`.
**Access to a local node is not necessary**, although it does simplify things. Koios can be used for
all steps that require access to a node.

Template bash scripts that follow these steps are available [here](scripts/). There are examples
using a local node and a remote node. Using a remote node requires extra steps since the transaction
must be manually balanced.

## Table of Contents
- [Installing](#installing)
- [Aiken For Developers](#aiken-for-developers)
- [Overspent Budget](#overspent-budget)
- [Using Remote Nodes](#using-remote-nodes)
- [Minting Test Tokens](#minting-test-tokens)
- [One-Way Swaps](#one-way-swaps)
  - [Registering The Beacon Script - DEVELOPERS
  ONLY](#registering-the-beacon-script---developers-only)
  - [Creating Reference Scripts](#creating-reference-scripts)
  - [Creating A Swap](#creating-a-swap)
  - [Closing A Swap](#closing-a-swap)
  - [Updating A Swap](#updating-a-swap)
  - [Converting A Swap To A New Trading Pair](#converting-a-swap-to-a-new-trading-pair)
  - [Executing A Swap](#executing-a-swap)
- [Two-Sway Swaps](#two-way-swaps)
  - [Registering The Beacon Script - DEVELOPERS
  ONLY](#registering-the-beacon-script---developers-only-1)
  - [Creating Reference Scripts](#creating-reference-scripts-1)
  - [Creating A Swap](#creating-a-swap-1)
  - [Closing A Swap](#closing-a-swap-1)
  - [Updating A Swap](#updating-a-swap-1)
  - [Converting A Swap To A New Trading Pair](#converting-a-swap-to-a-new-trading-pair-1)
  - [Executing A Swap](#executing-a-swap-1)
- [Querying](#querying)
  - [Personal Address](#personal-address)
  - [Own Swaps](#own-swaps)
  - [All Swaps](#all-swaps)


## Installing

Make sure `cardano-cli` is also installed. You can get the most up-to-date copy from IOG's
cardano-node repo [here](https://github.com/IntersectMBO/cardano-node/releases). It will be in the
cardano-node tarball under the latest release.

### Install the necessary packages - similar to cardano-node
```bash
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

### Install GHC 8.10.7 and cabal
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- Prepend or append the required PATH variable.
- You do not need to install the haskell-langauge-server.
- You do not need to install stack.
- Install the required packages. You can keep this terminal window open and install from another
window.
- Press ENTER to proceed.

```bash
source $HOME/.bashrc
ghcup install ghc 9.6.4
```

### Install libsodium, scep256k1, and blst
```bash
git clone https://github.com/intersectmbo/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../ # Leave the libsodium directory.
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig

cd ../ # Leave the secp256k1 directory.
git clone https://github.com/supranational/blst
cd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF # This command extends until the next EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

You need to execute the following to make the new packages usable:
```bash
echo '' >> $HOME/.bashrc # Add a newline to your .bashrc file.
echo 'export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"' >> $HOME/.bashrc
echo 'export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"' >> $HOME/.bashrc
source $HOME/.bashrc
```

### Build the executable - this may take about 30 minutes
```bash
cd ../ # Leave the blst directory.
git clone https://github.com/fallen-icarus/cardano-swaps
cd cardano-swaps
cabal clean
cabal update
cabal build exe:cardano-swaps
```

The `cardano-swaps` CLI program should now be at
`dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-swaps-1.0.0.0/x/cardano-swaps/build/cardano-swaps/cardano-swaps`.
Move the program to somewhere in your `$PATH`.

All `cardano-swaps` subcommands have an associated `--help` option. The functionality is meant to
feel like `cardano-cli`.

The smart contracts are compiled *into* the created `cardano-swaps` CLI. The executable has
everything you need for using the DEX. It is a batteries included CLI.

## Aiken For Developers

The aiken smart contracts come precompiled but if you would like to make changes or wish to confirm
the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using
cargo like this:

```bash
cargo install aiken --version 1.0.20-alpha
```

Make sure you instal verison 1.0.20-alpha. Newer versions may change some things and so the source
code may not compile or may result in a different script. As aiken stabilizes, the code will be
updated to the latest version.

> [!TIP] 
> If the above command doesn't work, you can build aiken from source:
> ```bash
> git clone https://github.com/aiken-lang/aiken
> cd aiken
> git checkout v1.0.20-alpha
> cargo build
> ```
> The executable should now be located at `target/debug/aiken`.

When building the protocol's blueprints, make sure to use

```bash
aiken build --keep-traces
```

or else the user friendly error messages will be stripped from the smart contracts and the resulting
beacons will be different.

For integration testing, you can create your own custom beacons without changing the protocol's
logic by changing the string passed [here](aiken/lib/cardano_swaps/common/types.ak#L4). Currently,
it is set to "". You can change this to any string personal to you so that you can get custom
beacons to play with for testing.

If you would like to use the `cardano-swaps` CLI after making your changes, you will need to
rebuild it with `cabal bulid exe:cardano-swaps`. As long as you did not make any breaking changes,
the CLI should still work for you.

If you would like to test your changes, you can run the tests using `cabal run tests`. As long
as you did not make any breaking changes, the tests should quickly give you feedback. There are
four kinds of tests:

1) Regression tests - tests for features that should work.
2) Failure tests - tests for scenarios that are supposed to fail.
3) Bench tests - tests to check for degraded performance in specific scenarios.
4) Performance Increase tests - tests to check for improved performance in specific scenarios; these
tests will fail if performance increases to alert you of the change.

To see the documentation for the tests, you can build the haddocks for the tests using `cabal
haddock tests`. The documentation may be easier to read than the source code. You can view the
documentation in any browser.

## Overspent Budget

While `cardano-cli` is able to auto-balance transactions, the auto-balancer does not work when
scripts are executed in a transaction where native tokens must go to the change address. It does not
properly add the change *before* estimating the execution budgets for the transaction which always
results in it under-estimating the required execution units needed by the scripts. There are open
issues about this [here](https://github.com/input-output-hk/cardano-node/issues/5386) and
[here](https://github.com/input-output-hk/cardano-api/issues/302). If you ever see a very long and
confusing error message about overspending budgets while using `cardano-cli transaction build`, this
is probably the issue.

As a work around, whenever you build a transaction using `cardano-cli transaction build` where
scripts are being executed, you must manually create an output that has all of the native tokens
that would normally go into the change output. You can let the auto-balancer balance the ada.

## Using Remote Nodes

`cardano-cli transaction build` requires a local node for the auto-balancer which means it cannot be
used to build a transaction. Instead, the `cardano-cli transaction build-raw` command is required.
This command requires the following steps:
1. Build a temporary transaction that is missing the execution units and transaciton fee but is
   properly balanced. You can assume a fee of zero for this transaction.
2. Submit the temporary transaction for execution budget estimations.
3. Rebuild the transaction with the proper execution budgets. The fee is still set to zero.
4. Calculate the required fee for this new temporary transaction.
5. Create the final transaction with the required fee and properly balanced outputs (subtract off
   the fee from the change).
6. Sign the transaction and submit to a remote node.

The `cardano-swaps` CLI uses [Koios](https://koios.rest/) in all scenarios where a node is required.

##### Exporting protocol parameters

Some of the above steps will require the current protocol parameters. The `cardano-swaps` CLI had
the preproduction testnet and mainnet protocol parameters compiled into the executable when it was
built with `cabal build exe:cardano-swaps`. The parameters are already formatted in the way
`cardano-cli` requires. To export the parameters, you can use:
```bash
cardano-swaps protocol-params \
  --testnet \
  --out-file protocolParams.json
```

##### Estimating execution budgets

Submitting a transaction for execution budget estimations can be done with this command:
```bash
cardano-swaps evaluate-tx \
  --testnet \
  --tx-file tx.body
```

This action uses Koios. The returned budgets will be indexed by the input order and policy id order.
**This may not be the same order you specified when building the temporary transaction.** The node
will reorder them base on lexicographical ordering. If you are not sure of the proper ordering, you
can view the transaction file that is created with `cardano-cli` using `cardano-cli transaction
view`; the inputs and policy ids will be properly ordered.

##### Submitting the final transaction

Submitting the final transaction for addition to the blockchain can be done with this command:
```bash
cardano-swaps submit \
  --testnet \
  --tx-file tx.signed
```

The transaction will be submitted through Koios.

## Minting Test Tokens

An always succeeding minting policy as well as the required redeemer are included with template bash
scripts for either a local node or a remote node. These can be used to create as many native tokens
as needed to test this DEX.

To see how to mint test tokens using a local node, refer 
[here](scripts/local/mint-test-tokens/mint.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/mint-test-tokens/mint.sh).


## One-Way Swaps

### Registering The Beacon Script - DEVELOPERS ONLY

**This action only needs to be done once for the entire DApp. It does not need to be done by any
users.** These instructions are for completeness as they may be needed by developers.

The beacon script cannot be executed as a staking script until after it is registered. Once the
script is registered, it can be used as a staking script immediately by all users. Registering the
script does not require executing the script. Once it is registered, *it cannot be deregistered*.

Registering the beacon script involves:
1. Exporting the beacon script from the `cardano-swaps` CLI.
2. Creating a registration certificate for the beacon script.
3. Submitting a transaction that also pays the registration deposit (2 ADA).

##### Exporting the beacon script
```bash
cardano-swaps scripts one-way beacon-policy \
  --out-file oneWayBeacons.plutus
```

##### Create the registration certificate
```bash
cardano-cli stake-address registration-certificate \
  --stake-script-file oneWayBeacons.plutus \
  --out-file registration.cert
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/register-beacon-script.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/register-beacon-script.sh).


### Creating Reference Scripts

**Don't skip this step!** While beacon tokens to be used to trustlessly share reference scripts,
this has not been set up for the beta testing. For now, you will need your own reference scripts.

Creating reference scripts involves the following steps:
1. Export the scripts from the `cardano-swaps` CLI.
2. Submit a transaction with the reference scripts stored in the outputs.

##### Exporting the scripts
```bash
# Export the spending script.
cardano-swaps scripts one-way swap-script \
  --out-file oneWaySwap.plutus

# Export the beacon script.
cardano-swaps scripts one-way beacon-script \
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
3. Create the required beacon script redeemer.
4. Create the required swap datum for each swap.
5. Submit a transaction that creates the swaps.

##### Creating your swap address
```bash
# Export the spending script.
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
One-way swaps require three beacons: the trading pair beacon, the offer beacon, and the ask beacon.

```bash
# Get the policy id for the one-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

# Get the required offer beacon name.
offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

# Get the required ask beacon name.
askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

# Create the required full beacon names.
pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"
```

The above beacons are for a swap that is offering a native token in exchange for ADA.

##### Create the required beacon script redeemer
```bash
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file beaconRedeemer.json
```

##### Creating the required swap datum
```bash
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --offer-price '1000000 / 1' \
  --out-file oneWaySwapDatum.json
```

**The price is always Ask/Offer.** In the above example, the swap wants 1 ADA per 1 native token
taken. The price can be specified as either a fraction (like above) or a decimal, such as:
`--offer-price 1000000`. Specifying a decimal may be more convenient but specifying the fraction
offers more control since it will be used on-chain as is (the decimal must be converted to a
fraction).

The `--input-swap-ref` flag is only needed when executing a swap; you can leave it out here.

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/create-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/create-swap.sh).


### Closing A Swap

Closing a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending script redeemer.
3. Calculate the required beacon names to burn.
4. Create the required beacon script redeemer.
5. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending script redeemer
```bash
cardano-swaps spending-redeemers one-way \
  --close \
  --out-file spendingRedeemer.json
```

##### Calculate the required beacon names to burn
```bash
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"
```

##### Create the required beacon script redeemer
```bash
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file beaconRedeemer.json
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/close-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/close-swap.sh).


### Updating A Swap

When no beacons need to be minted or burned, the beacon script can be executed as a staking script
for this action. If you are also converting a swap's pair or closing a swap in the transaction,
you do not need to use the staking execution; only the minting execution will be needed.

Updating a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending script redeemer.
3. Create the required beacon script redeemer.
4. Create the new swap datum.
5. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending script redeemer
```bash
cardano-swaps spending-redeemers one-way \
  --update-with-stake \
  --out-file spendingRedeemer.json
```

If the beacon script is being executed as a minting policy in this transaction, you can use the
`--update-with-mint` flag instead.

##### Create the beacon script redeemer.
```bash
cardano-swaps beacon-redeemers one-way \
  --update-only \
  --out-file beaconRedeemer.json
```

If the beacon script is going to be executed as a minting policy, use the `--mint-or-burn` flag
instead.

##### Creating the new swap datum
```bash
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --offer-price '1000000 / 2' \
  --out-file oneWaySwapDatum.json
```

**The price is always Ask/Offer.** In the above example, the swap wants 0.5 ADA per 1 native token
taken. The price can be specified as either a fraction (like above) or a decimal, such as:
`--offer-price 50000`. Specifying a decimal may be more convenient but specifying the fraction
offers more control since it will be used on-chain as is (the decimal must be converted to a
fraction).

The `--input-swap-ref` flag is only needed when executing a swap; you can leave it out here.

##### Building the transaction
The following transactions assume you are using a staking execution for the beacons script. If you
are going to use the minting execution instead, you can model the transactions off of the converting
swap template transactions.

To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/update-price.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/update-price.sh).


### Converting A Swap To A New Trading Pair

Converting a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending script redeemer.
3. Create the required beacon script redeemer.
4. Calculate the required beacon names to mint. These are for the new pair.
5. Calculate the required beacon names to burn. These are for the old pair.
6. Create the new swap datum.
7. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending script redeemer
```bash
cardano-swaps spending-redeemers one-way \
  --update-with-mint \
  --out-file spendingRedeemer.json
```

##### Create the required beacon script redeemer
```bash
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file beaconRedeemer.json
```

##### Calculate the required beacon names to mint
One-way swaps require three beacons: the trading pair beacon, the offer beacon, and the ask beacon.

```bash
# Get the policy id for the one-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
newPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

# Get the required offer beacon name.
newOfferBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

# Get the required ask beacon name.
newAskBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

# Create the required full beacon names.
newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newOfferBeacon="${beaconPolicyId}.${newOfferBeaconName}"
newAskBeacon="${beaconPolicyId}.${newAskBeaconName}"
```

The above beacons are for a swap that is offering a native token in exchange for ADA. 

##### Calculate the required beacon names to burn

```bash
# Get the policy id for the one-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
oldPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

# Get the required offer beacon name.
oldOfferBeacon=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

# Get the required ask beacon name.
oldAskBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

# Create the required full beacon names.
oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldOfferBeacon="${beaconPolicyId}.${oldOfferBeacon}"
oldAskBeacon="${beaconPolicyId}.${oldAskBeaconName}"
```

Since the `oldAskBeacon` is the same as the `newAskBeacon`, the old one does not actually need to be
burned. This example is just for completeness.

##### Creating the new swap datum
```bash
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --price-numerator 50000 \
  --out-file oneWaySwapDatum.json
```

**The price is always Ask/Offer.** In the above example, the swap wants 0.5 ADA per 1 native token
taken. The price can be specified as either a fraction (like above) or a decimal. Specifying a
decimal may be more convenient but specifying the fraction offers more control since it will be used
on-chain as is (the decimal must be converted to a fraction).

The `--input-swap-ref` flag is only needed when executing a swap; you can leave it out here.

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/convert-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/convert-swap.sh).



### Executing A Swap

Executing a swap involves the following steps:
1. Create the spending script redeemer.
2. Create the corresponding swap datum for the target swap.
3. Submit the transaction.

It may be helpful to also calculate the beacon names and store them as variables since the swap
output still needs them.

##### Create the spending script redeemer
```bash
cardano-swaps spending-redeemers one-way \
  --swap \
  --out-file swap.json
```

##### Create the corresponding swap datum for the target swap.
```bash
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --offer-price '50000 / 1' \
  --input-swap-ref 71ae3f66eead7198a79232ff8f2c032d845d0070d3f066f1b5dec3c2abe99788#0 \
  --out-file oneWaySwapDatum.json
```

The datum should be *exactly* the same as the target swap's datum except the new datum should point
to the swap input: the `--input-swap-ref` flag should specify the UTxO of the swap being consumed.

Since the price in the swap datum must be exact, it is better to specify the price as a fraction.


##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/one-way/swap-assets.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/one-way/swap-assets.sh).


## Two-Way Swaps

### Registering The Beacon Script - DEVELOPERS ONLY

**This action only needs to be done once for the entire DApp. It does not need to be done by any
users.** These instructions are for completeness as they may be needed by developers.

The beacon script cannot be executed as a staking script until after it is registered. Once the
script is registered, it can be used as a staking script immediately by all users. Registering the
script does not require executing the script. Once it is registered, *it cannot be deregistered*.

Registering the beacon script involves:
1. Exporting the beacon script from the `cardano-swaps` CLI.
2. Creating a registration certificate for the beacon script.
3. Submitting a transaction that also pays the registration deposit.

##### Exporting the beacon script
```bash
cardano-swaps scripts two-way beacon-policy \
  --out-file twoWayBeacons.plutus
```

##### Create the registration certificate
```bash
cardano-cli stake-address registration-certificate \
  --stake-script-file twoWayBeacons.plutus \
  --out-file registration.cert"
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/register-beacon-script.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/register-beacon-script.sh).


### Creating Reference Scripts

**Don't skip this step!** While beacon tokens to be used to trustlessly share reference scripts,
this has not been set up for the beta testing. For now, you will need your own reference scripts.

Creating reference scripts involves the following steps:
1. Export the scripts from the `cardano-swaps` CLI.
2. Submit a transaction with the reference scripts stored in the outputs.

##### Exporting the scripts
```bash
# Export the spending script.
cardano-swaps scripts two-way swap-script \
  --out-file twoWaySwap.plutus

# Export the beacon script.
cardano-swaps scripts two-way beacon-policy \
  --out-file twoWayBeacons.plutus
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/create-reference-scripts.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/create-reference-scripts.sh).

### Creating A Swap

Creating a swap involves the following steps:
1. Create your swap address.
2. Calculate the required beacon names to mint.
3. Create the required beacon script redeemer.
4. Create the required swap datum for each swap.
5. Submit a transaction that creates the swaps.

##### Creating your swap address
```bash
# Export the spending script.
cardano-swaps scripts two-way swap-script \
  --out-file twoWaySwap.plutus

# Create the swap address.
cardano-cli address build \
  --payment-script-file twoWaySwap.plutus \
  --stake-verification-key-file ownerStake.vkey \
  --testnet-magic 1 \
  --out-file twoWaySwap.addr
```

Cardano-Swaps also supports using staking scripts for the address. To use a staking script, use
the `--stake-script-file` flag instead of the `--stake-verification-key-file` flag.

For a mainnet address, just use the `--mainnet` flag instead of `--testnet-magic 1` when creating
the address.

##### Calculate the required beacon names to mint
Two-way swaps require three beacons: the trading pair beacon, the asset1 beacon, and the asset2
beacon.

```bash
# Get the policy id for the two-way swap beacons.
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

# Get the required trading pair beacon name.
pairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

# Get the required asset1 beacon name.
asset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

# Get the required asset2 beacon name.
asset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

# Create the required full beacon names.
pairBeacon="${beaconPolicyId}.${pairBeaconName}"
asset1Beacon="${beaconPolicyId}.${asset1BeaconName}"
asset2Beacon="${beaconPolicyId}.${asset2BeaconName}"
```

The above beacons are for a two-way swap between ADA and a native token. Which asset is specified
as first or second is irrelevant, the CLI will properly create the beacons.

##### Create the required beacon script redeemer
```bash
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file beaconRedeemer.json
```

##### Creating the required swap datum
```bash
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --first-price '1 / 1000000' \
  --second-price '1500000 / 1' \
  --out-file twoWaySwapDatum.json
```

**The prices are always Ask/Offer from the perspective of the swap.** In other words, the numerators
are always the amount asked for and the denominator is the amount that is allow to be taken.

Which asset is first or second is irrelevant; all that matters is that the `first-price` corresponds
to the `first-asset`, and the `second-price` corresponds to the `second-asset`. The CLI will handle
the rest.

- `first-price` = second asset / first asset. Therefore, the above example's `first-price` is 1
native token given for every 1 ADA taken.
- `second-price` = first asset / second asset. Therefore, the above example's `second-price` is 1.5
ADA given for every 1 native token taken.

The `--input-swap-ref` flag is only needed when executing a swap; you can leave it out here.

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/create-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/create-swap.sh).


### Closing A Swap

Closing a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending script redeemer.
3. Calculate the required beacon names to burn.
4. Create the required beacon script redeemer.
5. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending script redeemer
```bash
cardano-swaps spending-redeemers two-way \
  --close \
  --out-file spendingRedeemer.json
```

##### Calculate the required beacon names to burn
```bash
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

asset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
asset1Beacon="${beaconPolicyId}.${asset1BeaconName}"
asset2Beacon="${beaconPolicyId}.${asset2BeaconName}"
```

The above beacons are for a two-way swap between ADA and a native token. Which asset is specified
as first or second is irrelevant, the CLI will properly create the beacons.

##### Create the required beacon script redeemer
```bash
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file burnTwoWayBeacons.json
```

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/close-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/close-swap.sh).


### Updating A Swap

When no beacons need to be minted or burned, the beacon script can be executed as a staking script
for this action. If you are also converting a swap's pair or closing a swap in the transaction,
you do not need to use the staking execution; only the minting execution will be needed.

Updating a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending redeemer.
3. Create the required beacon script redeemer.
4. Create the new swap datum.
5. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending script redeemer
```bash
cardano-swaps spending-redeemers two-way \
  --update-with-mint \
  --out-file spendingRedeemer.json
```

If the beacon script is being executed as a minting policy in this transaction, you can use the
`--update-with-mint` flag instead.

##### Create the beacon script redeemer.
```bash
cardano-swaps beacon-redeemers one-way \
  --update-only \
  --out-file beaconRedeemer.json
```

If the beacon script is going to be executed as a minting policy, use the `--mint-or-burn` flag
instead.

##### Creating the new swap datum
```bash
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --second-price 1000000 \
  --first-price '2 / 1000000' \
  --out-file twoWaySwapDatum.json
```

**The prices are always Ask/Offer from the perspective of the swap.** In other words, the numerators
are always the amount asked for and the denominator is the amount that is allow to be taken.

Which asset is first or second is irrelevant; all that matters is that the `first-price` corresponds
to the `first-asset`, and the `second-price` corresponds to the `second-asset`. The CLI will handle
the rest.

- `first-price` = second asset / first asset. Therefore, the above example's `first-price` is 2
native token given for every 1 ADA taken.
- `second-price` = first asset / second asset. Therefore, the above example's `second-price` is 1
ADA given for every 1 native token taken.

The `--input-swap-ref` flag is only needed when executing a swap; you can leave it out here.

##### Building the transaction
The following transactions assume you are using a staking execution for the beacons script. If you
are going to use the minting execution instead, you can model the transactions off of the converting
swap template transactions.

To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/update-price.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/update-price.sh).


### Converting A Swap To A New Trading Pair

Converting a swap requires the following steps:
1. Calculate the hash of the swap's staking credential.
2. Create the required spending script redeemer.
3. Create the required beacon script redeemer.
4. Calculate the required beacon names to mint. These are for the new pair.
5. Calculate the required beacon names to burn. These are for the old pair.
6. Create the new swap datum.
7. Submit the transaction.

##### Calculate the hash of the swap's staking credential
```bash
# Generate the hash for a staking verification key.
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey)

# Generate the hash for a staking script.
ownerStakingScriptHash=$(cardano-cli transaction policyid \
  --script-file ownerStake.plutus)
```

While the `cardano-cli transaction policyid` command is meant for minting policies, it works for
creating the hash of *any* script.

##### Create the required spending script redeemer
```bash
cardano-swaps spending-redeemers two-way \
  --close-or-update \
  --out-file spendingRedeemer.json
```

##### Create the required beacon script redeemer
```bash
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file beaconRedeemer.json
```

This redeemer can be used to both burn the old beacons and mint the new ones.

##### Calculate the required beacon names to mint
Two-way swaps require three beacons: the trading pair beacon, an asset1 beacon, and an asset2
beacon.

```bash
newPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

newAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newAsset1Beacon="${beaconPolicyId}.${newAsset1BeaconName}"
newAsset1Beacon="${beaconPolicyId}.${newAsset2BeaconName}"
```

The above beacons are for a two-way swap between ADA and a native token. Which asset is specified
as first or second is irrelevant, the CLI will properly create the beacons.

##### Calculate the required beacon names to burn

```bash
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

oldPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

oldAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldAsset1Beacon="${beaconPolicyId}.${oldAsset1BeaconName}"
oldAsset2Beacon="${beaconPolicyId}.${oldAsset2BeaconName}"
```

The above beacons are for a two-way swap between ADA and a native token. Which asset is specified
as first or second is irrelevant, the CLI will properly create the beacons.

##### Creating the new swap datum
```bash
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --second-price 1000000 \
  --first-price '2 / 1000000' \
  --out-file twoWaySwapDatum.json
```

**The prices are always Ask/Offer from the perspective of the swap.** In other words, the numerators
are always the amount asked for and the denominator is the amount that is allow to be taken.

Which asset is first or second is irrelevant; all that matters is that the `first-price` corresponds
to the `first-asset`, and the `second-price` corresponds to the `second-asset`. The CLI will handle
the rest.

- `first-price` = second asset / first asset. Therefore, the above example's `first-price` is 2
native token given for every 1 ADA taken.
- `second-price` = first asset / second asset. Therefore, the above example's `second-price` is 1
ADA given for every 1 native token taken.

The `--input-swap-ref` flag is only needed when executing a swap; you can leave it out here.


##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/convert-swap.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/convert-swap.sh).


### Executing A Swap

Executing a swap involves the following steps:
1. Create the spending script redeemer.
2. Create the corresponding swap datum for the target swap.
3. Submit the transaction.

It may be helpful to also calculate the beacon names and store them as variables since the swap
output still needs them.

- `TakeAsset1` means asset1 is being taken from the swap and asset2 is being deposited.
- `TakeAsset2` means asset2 is being taken from the swap and asset1 is being deposited.

**Which asset in the swap is asset1 and which is asset2 depends on the lexicographical ordering of
the asset names.**

##### Create the spending script redeemer
```bash
cardano-swaps spending-redeemers two-way \
  --take-asset1 \
  --out-file twoWaySwapRedeemer.json
```

To take asset2, use the `--take-asset2` flag instead.

If you do not know what swap direction is required because you do not know the proper
lexicographical ordering of the assets, you can tell the `cardano-swaps` CLI what the offer asset
and ask asset are and it can create the proper swap redeemer for you:

```bash
cardano-swaps spending-redeemers two-way \
  --offer-asset lovelace \
  --ask-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --out-file twoWaySwapRedeemer.json
```

**The offer and ask are always from the perspective of the swap UTxO.** So if you are trying to take
ada from the swap and deposit a native token, the offer asset is ada and the ask asset is the native
token.


##### Create the corresponding swap datum for the target swap.
```bash
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --first-price '10 / 1000000' \
  --second-price '1500000 / 1' \
  --input-swap-ref 61e92820c602d7d4b388140174e2ed76a924541b08a57072bc79c003b84d5a01#0 \
  --out-file twoWaySwapDatum.json
```

The datum should be *exactly* the same as the target swap's datum except the new datum should point
to the swap input: the `--input-swap-ref` flag should specify the UTxO of the swap being consumed.

Since the prices in the swap datum must be exact, it is better to specify the prices as fractions.

##### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local/two-way/swap-assets.sh).

To see how to build the transaction using a remote node, refer
[here](scripts/remote/two-way/swap-assets.sh).


## Querying

- All queries use Koios.
- All query commands are capable of saving results to a file or printing to stdout. 
- Results can be formatted as JSON, pretty, or plain. 

The pretty and plain formats are meant for printing to the stdout but both can also be saved to a
file. The only difference between the pretty format and the plain format is the pretty format uses
ansii escape sequences to highlight certain items with color. The plain format is there as a
fallback in case the ansii escape sequences are causing issues for a user.

> Note: Currently, the `cardano-swaps` CLI will only get the first 1000 UTxOs that satisfy a query.
> This could be 1000 personal UTxOs or 1000 swap UTxOs, depending on the query. For the beta
> release, 1000 should be plenty. The CLI will be expanded in the future to remove this cap.

### Personal Address

In order to facilitate the use of remote nodes, `cardano-swaps` is capable of querying personal
addresses.

The command is simply:
```bash
cardano-swaps query personal-address \
  --testnet \
  --address $(cat personal.addr) \
  --pretty \
  --stdout
```

The UTxOs will be returned in lexicographical ordering based on the transaction hash.

For the pretty and plain formats, UTxOs that contain a reference script and/or a datum will show the
script hash or datum hash, respectively. For the pretty format, each type of hash will have a color
associated with it: Blue for script hashes and Green for datum hashes. UTxO assets are always shown.

### Own Swaps

The `cardano-swaps query own-swaps` command can be used to query your own swaps. The swaps can be
filtered by offer asset, ask asset, or by trading pair if desired. The results are not sorted in any
way.

*Make sure to specify the right address with the command; if you specify the two-way swap address
when using the one-way query, you may get unexpected results.*

##### Query One-Way Swaps By Offer
```bash
cardano-swaps query own-swaps one-way offer \
  --testnet \
  --address $(cat oneWaySwap.addr) \
  --offer-asset lovelace \
  --pretty \
  --stdout
```

##### Query Two-Way Swaps By Offer
```bash
cardano-swaps query own-swaps two-way offer \
  --testnet \
  --address $(cat twoWaySwap.addr) \
  --offer-asset lovelace \
  --pretty \
  --stdout
```

Since both asset1 and asset2 can be the offer asset for two-way swaps depending on the swap's
direction, this query requires you to choose one of the assets to be the offer asset.

##### Query One-Way Swaps By Ask
```bash
cardano-swaps query own-swaps one-way offer \
  --testnet \
  --address $(cat oneWaySwap.addr) \
  --ask-asset lovelace \
  --pretty \
  --stdout
```

##### Query Two-Way Swaps By Ask
```bash
cardano-swaps query own-swaps two-way ask \
  --testnet \
  --address $(cat twoWaySwap.addr) \
  --ask-asset lovelace \
  --pretty \
  --stdout
```

Since both asset1 and asset2 can be the ask asset for two-way swaps depending on the swap's
direction, this query requires you to choose one of the assets to be the ask asset.

##### Query One-Way Swaps By Trading Pair
```bash
cardano-swaps query own-swaps one-way trading-pair \
  --testnet \
  --address $(cat oneWaySwap.addr) \
  --offer-asset lovelace \
  --ask-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --pretty \
  --stdout
```

##### Query Two-Way Swaps By Trading Pair
```bash
cardano-swaps query own-swaps two-way trading-pair \
  --testnet \
  --address $(cat twoWaySwap.addr) \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --pretty \
  --stdout
```

The assets do not need to be properly sorted for this query to work. The CLI will
properly query the swaps regardless of which asset is `first` and `second`.

### All Swaps

Querying all swaps will query both one-way swaps and two-ways swaps. The results will specify
whether the swap is a one-way swap or a two-way swap.

The `cardano-swaps query all-swaps` command is used to query all swaps. Swaps can be queried based
on the offer asset, the ask asset, or the trading pair. (It is technically also possible to query
all swaps but that is not supported by `cardano-swaps` CLI since it does not seem like a useful
query. If you think it would be useful, feel free to open an issue.)

##### Swaps By Trading Pair 

When querying all swaps by trading pair, a swap direction *must* be specified. The returned swaps
are sorted based on the prices from lowest to highest (taking into account whether that
direction requires the `asset1Price` or `asset2Price` for two-way swaps).

When the pretty format is used, the relevant price is highlighted in Magenta.

```bash
cardano-swaps query all-swaps trading-pair \
  --testnet \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --pretty \
  --stdout
```

##### Swaps By Offer

The returned swaps are *not* sorted nor are the prices highlighted in the pretty format.

```bash
cardano-swaps query all-swaps offer \
  --testnet \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --pretty \
  --stdout
```

##### Swaps By Ask

The returned swaps are *not* sorted nor are the prices highlighted in the pretty format.

```bash
cardano-swaps query all-swaps ask \
  --testnet \
  --ask-asset lovelace \
  --pretty \
  --stdout
```
