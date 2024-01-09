#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

beaconScriptFile="${dir}twoWayBeacons.plutus"
beaconAddrFile="${dir}twoWayBeaconStake.addr"

swapAddrFile="${dir}twoWaySwap.addr"

swapDatumFile="${dir}swapDatum.json"

swapRedeemerFile="${dir}twoWaySpendingRedeemer.json"
beaconRedeemerFile="${dir}twoWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts two-way beacon-script \
  --out-file $beaconScriptFile

# Create the meta beacon stake address.
echo "Creating the beacon reward address..."
cardano-cli stake-address build \
  --stake-script-file $beaconScriptFile \
  --testnet-magic 1 \
  --out-file $beaconAddrFile

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --update-with-stake \
  --out-file $swapRedeemerFile

# Create the beacon script redeemer.
echo "Creating the beacon redeemer..."
cardano-swaps beacon-redeemers two-way \
  --update-only \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --second-asset lovelace \
  --first-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --second-price '2 / 1000000' \
  --first-price 2000000 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

asset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

asset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
asset1Beacon="${beaconPolicyId}.${asset1BeaconName}"
asset2Beacon="${beaconPolicyId}.${asset2BeaconName}"

# Create the transaction.
cardano-cli transaction build \
  --tx-in 22e60774851d6db2c2e4600e3c6daebe3b68d52e1d9d7070f6c2fc6ee8c8efcf#1 \
  --tx-in 22e60774851d6db2c2e4600e3c6daebe3b68d52e1d9d7070f6c2fc6ee8c8efcf#0 \
  --spending-tx-in-reference 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#1 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${asset1Beacon} + 1 ${asset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
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
