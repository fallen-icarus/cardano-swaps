#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapAddr1="addr_test1zzrns8ct7stw9kh8f97nlnvqsl8kw7eukje2aw3kak8c77fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq3y05nq"
swapDatumFile1="${tmpDir}swapDatum1.json"
swapRedeemerFile="${tmpDir}oneWaySpendingRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
spendingScriptPreprodTestnetRef="115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#0"
# spendingScriptSize=5343

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --first-price '1 / 1000000' \
  --second-price 1000000 \
  --input-swap-ref f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Create the transaction.
cardano-cli conway transaction build \
  --tx-in 8064545d5c06fcd051eedf4f2d5a2e6efdc08b376720f21b1b3457d48bb536e1#2 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "${swapAddr1} + 13000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
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
