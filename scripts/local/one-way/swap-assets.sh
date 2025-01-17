#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapAddr1="addr_test1zqql5djxthlrdcnvy87m7uswf0d0es9cdw6nvl72gcqj743ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqv7yg2s"
swapDatumFile1="${tmpDir}swapDatum1.json"
swapRedeemerFile="${tmpDir}oneWaySpendingRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
spendingScriptPreprodTestnetRef="9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#0"
# spendingScriptSize=4842

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --swap \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --offer-price 1000000 \
  --input-swap-ref 8f224fb4d358bfc9c05b1784d412c2b9161d66a2246734a400a52578ceef26f5#0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

offerBeaconName1=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

askBeaconName1=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
offerBeacon1="${beaconPolicyId1}.${offerBeaconName1}"
askBeacon1="${beaconPolicyId1}.${askBeaconName1}"

# Create the transaction.
cardano-cli conway transaction build \
  --tx-in 67b735bc6f14e5639ad49ad226a052281cf96616ba9b1dfc498e75e6ecca09d1#0 \
  --tx-in 8f224fb4d358bfc9c05b1784d412c2b9161d66a2246734a400a52578ceef26f5#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "${swapAddr1} + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${offerBeacon1} + 1 ${askBeacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --change-address "$(cat $HOME/wallets/02.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
