#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapAddr1="addr_test1zqge9z36cwm9akl3q04xhvek9cums7drdupgjl0nr3qfz7eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqrvvqgp"
swapDatumFile1="${tmpDir}swapDatum1.json"
swapRedeemerFile="${tmpDir}oneWaySpendingRedeemer.json"

# The time the swap expires.
expirationTime=1761840180000
expirationSlot=$(cardano-swaps time convert-time --testnet --posix-time $expirationTime)

# The reference scripts may already be locked on-chain. Check the two-way swap address without a
# staking credential. Both the spending script and the beacon script will be permanently locked in
# this address.
#
# cardano-cli conway address build \
#   --payment-script-file $swapScriptFile \
#   --testnet-magic 1 \
#   --out-file $swapAddrFile
#
# You can use the `cardano-swaps query personal-address` command to see them.

spendingScriptPreprodTestnetRef="9415db73d8d374572a58ad167e3051110251aff802987f8627b27e060dcd673f#0"
# spendingScriptSize=5007

# Create the Swap redeemer.
# If you are unsure which direction you need, you can specify which assets the swap is
# asking/offering. So asking for ADA means you can deposit ADA to claim the other asset.
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
  --second-price '2000000 / 1' \
  --input-swap-ref 7b113cebcbd43339b38be9aaafa676ae7e2af14cf35fc32f720bcd33e7359bd1#0 \
  --expiration $expirationTime \
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
  --tx-in 4f8c4a3bceab49f65dfd49d47244042d5162e06e6deea49d7e9baa9028d4287c#2 \
  --tx-in 4ff289b54b595f621b12ca03e7425e178a029aa0fe487984570879f1ba538a08#0 \
  --tx-in 7b113cebcbd43339b38be9aaafa676ae7e2af14cf35fc32f720bcd33e7359bd1#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "${swapAddr1} + 13000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --change-address "$(cat $HOME/wallets/02.addr)" \
  --invalid-hereafter $expirationSlot \
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
