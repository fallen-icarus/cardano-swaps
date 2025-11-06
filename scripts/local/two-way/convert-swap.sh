#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

ownerPubKeyFile="$HOME/wallets/01Stake.vkey"
swapAddrFile="${tmpDir}twoWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
swapRedeemerFile="${tmpDir}twoWaySpendingRedeemer.json"
beaconRedeemerFile="${tmpDir}twoWayBeaconRedeemer.json"

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

beaconScriptPreprodTestnetRef="9415db73d8d374572a58ad167e3051110251aff802987f8627b27e060dcd673f#1"
# beaconScriptSize=4804

spendingScriptPreprodTestnetRef="9415db73d8d374572a58ad167e3051110251aff802987f8627b27e060dcd673f#0"
# spendingScriptSize=5007

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the owner..."
ownerPubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --update-with-mint \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."

# The expiration will be set 1 hr from now:
currentSlot=$(cardano-swaps query current-slot --testnet)
tmpExpirationSlot=$((currentSlot + 3600))
tmpExpirationTime=$(cardano-swaps time convert-time --slot $tmpExpirationSlot --testnet)

# The time must be rounded to the nearest minute.
expirationTime=$(cardano-swaps time round-to-min --posix-time $tmpExpirationTime)
# We need the corresponding slot to the rounded time for tx validity interval.
expirationSlot=$(cardano-swaps time convert-time --posix-time $expirationTime --testnet)

# Create the datum. The expiration field is optional.
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --second-price 1000000 \
  --first-price '1 / 1000000' \
  --expiration $expirationTime \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

oldPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

oldAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

oldAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldAsset1Beacon="${beaconPolicyId}.${oldAsset1BeaconName}"
oldAsset2Beacon="${beaconPolicyId}.${oldAsset2BeaconName}"

newPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

newAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

newAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newAsset1Beacon="${beaconPolicyId}.${newAsset1BeaconName}"
newAsset2Beacon="${beaconPolicyId}.${newAsset2BeaconName}"

# Create the beacon script redeemer. 
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction. If you are creating a swap that expires, you must set
# `invalid-hereafter` to the nearest expiration slot.
cardano-cli conway transaction build \
  --tx-in 74a88e679036309f21154746ddc813c97f5d5bb0e4ebaa0244997ac13e44eb77#1 \
  --tx-in 397ceab35a092814871ba5bf9a946a55508afee4edc26c8728caa53837b67fcf#1 \
  --tx-in 74a88e679036309f21154746ddc813c97f5d5bb0e4ebaa0244997ac13e44eb77#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --change-address "$(cat $HOME/wallets/01.addr)" \
  --required-signer-hash "$ownerPubKeyHash" \
  --invalid-hereafter $expirationSlot \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --signing-key-file $HOME/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
