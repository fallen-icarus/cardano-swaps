#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapAddr1="addr_test1zzje7ushtlzmyu62qtpsf02dlge0ddkeutlu6avln3vqgx3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq5ajzkh"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}swap.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --swap \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --price-numerator 2000000 \
  --price-denominator 1 \
  --tx-hash 278d7c43175cc27b89962ce80d23851ad6353f79239e9df3b51213afa5aa9204 \
  --output-index 0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

offerBeaconName1=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

askBeaconName1=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-lovelace \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
offerBeacon1="${beaconPolicyId1}.${offerBeaconName1}"
askBeacon1="${beaconPolicyId1}.${askBeaconName1}"

# Create the transaction.
cardano-cli transaction build \
  --tx-in 8498f8e46ed2d1edef10ed90be4adf2a3d1620e6164ce6a5ca513d983c430c16#1 \
  --tx-in 278d7c43175cc27b89962ce80d23851ad6353f79239e9df3b51213afa5aa9204#0 \
  --spending-tx-in-reference 3d91a6c59c4065c8b9882a7e232824d2064e92024d0db318f09b6ad815f1ccd4#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "${swapAddr1} + 7000000 lovelace + 1 ${pairBeacon1} + 1 ${offerBeacon1} + 1 ${askBeacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
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
