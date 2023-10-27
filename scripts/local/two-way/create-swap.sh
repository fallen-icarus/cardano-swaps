#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}two_way_spend.plutus" # This is used to create the swap address.

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}two_way_swap.addr"

swapDatumFile1="${dir}swap_datum1.json"
swapDatumFile2="${dir}swap_datum2.json"

beaconRedeemerFile="${dir}create_two_way_swap.json"

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

# Create the swap address.
echo "Creating the swap address..."
cardano-cli address build \
  --payment-script-file $swapScriptFile \
  --stake-verification-key-file $ownerPubKeyFile \
  --testnet-magic 1 \
  --out-file $swapAddrFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Get the mint beacon redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --create-swap \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --forward-price-numerator 1000000 \
  --forward-price-denominator 1 \
  --reverse-price-numerator 1 \
  --reverse-price-denominator 1000000 \
  --out-file $swapDatumFile1

# Create the transaction.
echo "Building the transaction..."
cardano-cli transaction build \
  --tx-in 5ae04fb39873e137cfbffcdecedbad4908d8665d53994c8aa56837893be9341d#2 \
  --tx-in 70571a904a6e4228c5f8f31414cfd90e563c1babf638c1016b54b7c433cce397#1 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 280 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "2 ${pairBeacon1} + 2 ${asset1Beacon1} + 2 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --change-address "$(cat ../../../ignored/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
