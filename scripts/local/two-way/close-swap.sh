#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapRedeemerFile="${dir}twoWaySpendingRedeemer.json"

beaconRedeemerFile="${dir}twoWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --close \
  --out-file $swapRedeemerFile

# Helper beacon variables.
echo "Calculating the beacon names..."
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

# Creating the beacon script redeemer.  
echo "Creating the beacon script redeemer..."
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli transaction build \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#2 \
  --tx-in a3ceb781e7d8429eb72e34e9219a99918dbb01db67dbeaf97a4ab5d5f893c873#0 \
  --spending-tx-in-reference 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "-1 ${pairBeacon} + -1 ${asset1Beacon} + -1 ${asset2Beacon}" \
  --mint-tx-in-reference 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --change-address "$(cat ../../../ignored/wallets/01.addr)" \
  --required-signer-hash "$ownerPubKeyHash" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --signing-key-file ../../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
