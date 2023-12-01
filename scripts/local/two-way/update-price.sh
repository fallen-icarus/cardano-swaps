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
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --forward-price-numerator 1000000 \
  --forward-price-denominator 1 \
  --reverse-price-numerator 2 \
  --reverse-price-denominator 1000000 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --stdout)

asset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
asset1Beacon="${beaconPolicyId}.${asset1BeaconName}"
asset2Beacon="${beaconPolicyId}.${asset2BeaconName}"

# Create the transaction.
cardano-cli transaction build \
  --tx-in 825a452672dff10a4ae463caa9c53cce428658b04a53a299e227fff87286757f#2 \
  --tx-in 303fdb0c961c9d20dfd741f7b0d4979868f0f14e355bdf13596941f105c59e34#0 \
  --spending-tx-in-reference 825a452672dff10a4ae463caa9c53cce428658b04a53a299e227fff87286757f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference 825a452672dff10a4ae463caa9c53cce428658b04a53a299e227fff87286757f#1 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${asset1Beacon} + 1 ${asset2Beacon} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
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
