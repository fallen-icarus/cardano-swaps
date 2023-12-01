#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapAddr1="addr_test1zrd7gr0cwkclrhxq4p6m7qnwha77sk7adrc873wym5em40eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqsq4qg4"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --swap \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --price-numerator 1000000 \
  --price-denominator 1 \
  --tx-hash 1222c97808e9fb8b2212d0d337f6744072c74cb1cae660e0160e5bbd97fc434f \
  --output-index 0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

offerBeaconName1=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

askBeaconName1=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-lovelace \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
offerBeacon1="${beaconPolicyId1}.${offerBeaconName1}"
askBeacon1="${beaconPolicyId1}.${askBeaconName1}"

# Create the transaction.
cardano-cli transaction build \
  --tx-in dfcdd809ffafc52112e68e734f6ca1603aab6c02317579254eb89a84570ca63d#1 \
  --tx-in 1222c97808e9fb8b2212d0d337f6744072c74cb1cae660e0160e5bbd97fc434f#0 \
  --spending-tx-in-reference 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "${swapAddr1} + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${offerBeacon1} + 1 ${askBeacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
