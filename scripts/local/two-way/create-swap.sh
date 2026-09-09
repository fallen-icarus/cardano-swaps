#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapScriptFile="${tmpDir}twoWaySwap.plutus" # This is used to create the swap address.
ownerPubKeyFile="$HOME/wallets/01Stake.vkey"

# The staking credential must approve the transaction that creates the swaps.
ownerPubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)
swapAddrFile="${tmpDir}twoWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
beaconRedeemerFile="${tmpDir}twoWaySwapBeaconRedeemer.json"

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

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

# Create the swap address.
echo -n "Creating the swap address... "
cardano-cli conway address build \
  --payment-script-file $swapScriptFile \
  --stake-verification-key-file $ownerPubKeyFile \
  --testnet-magic 1 \
  --out-file $swapAddrFile

cat $swapAddrFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

asset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
asset1Beacon="${beaconPolicyId}.${asset1BeaconName}"
asset2Beacon="${beaconPolicyId}.${asset2BeaconName}"

# Get the mint beacon redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

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
  --first-price '1 / 1000000' \
  --second-price 2000000 \
  --expiration $expirationTime \
  --out-file $swapDatumFile

# Create the transaction. If you are creating a swap that expires, you must set
# `invalid-hereafter` to the nearest expiration slot.
echo "Building the transaction..."
cardano-cli conway transaction build \
  --tx-in 96244b62d74ba9cfd8f1ad9094be8a5f729c300183637f49d40a2c8c84e12673#0 \
  --tx-in 95e9112380578f7a7e8144346206de56f2144e45c58da6580fa8bcdaa65fd4ab#1 \
  --tx-in 94cc87dc7ec40a8cd88cb8770d7b04096dd8a9ab17428cc2e1980f3f616dc213#0 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${asset1Beacon} + 1 ${asset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --mint "1 ${pairBeacon} + 1 ${asset1Beacon} + 1 ${asset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v3 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --required-signer-hash "$ownerPubKeyHash" \
  --change-address "$(cat $HOME/wallets/01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
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
