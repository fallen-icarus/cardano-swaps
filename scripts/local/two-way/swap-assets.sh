#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapAddr1="addr_test1zzseu072la47qt0mct984lqjmp37sn5gyv4x2vam42gum6pualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqqgram2"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --out-file $swapRedeemerFile

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
  --tx-hash ad859315e67109e3a764d3cd331b972514469c19c34658a78e7202475478b806 \
  --output-index 0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way asset-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way asset-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Create the transaction.
cardano-cli transaction build \
  --tx-in f5e9cb3a3c2d41ba6ae15e62c9a8dafdb9a62e821acb7d8f328fe3429306b0b0#2 \
  --tx-in ad859315e67109e3a764d3cd331b972514469c19c34658a78e7202475478b806#0 \
  --spending-tx-in-reference 825a452672dff10a4ae463caa9c53cce428658b04a53a299e227fff87286757f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "${swapAddr1} + 7000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --change-address "$(cat ../../../ignored/wallets/02.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
