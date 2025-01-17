#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

ownerPubKeyFile="$HOME/wallets/01Stake.vkey"
beaconScriptFile="${tmpDir}oneWayBeacons.plutus"
beaconAddrFile="${tmpDir}oneWayBeaconStake.addr"
swapAddrFile="${tmpDir}oneWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
swapRedeemerFile="${tmpDir}oneWaySpendingRedeemer.json"
beaconRedeemerFile="${tmpDir}oneWayBeaconRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
beaconScriptPreprodTestnetRef="9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#1"
# beaconScriptSize=4432

spendingScriptPreprodTestnetRef="9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#0"
# spendingScriptSize=4842

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconScriptFile

# Create the meta beacon stake address.
echo "Creating the beacon reward address..."
cardano-cli conway stake-address build \
  --stake-script-file $beaconScriptFile \
  --testnet-magic 1 \
  --out-file $beaconAddrFile

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --update-with-stake \
  --out-file $swapRedeemerFile

# Create the beacon script redeemer.
echo "Creating the beacon redeemer..."
cardano-swaps beacon-redeemers one-way \
  --update-only \
  --out-file $beaconRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --offer-price 2000000 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"

# Create the transaction.
cardano-cli conway transaction build \
  --tx-in 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#2 \
  --tx-in 44101845b0301455ec2a3dd7b98a3b22623011fb38a6216ae1fa78358c5a61fc#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference $beaconScriptPreprodTestnetRef \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 15 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --change-address "$(cat $HOME/wallets/01.addr)" \
  --required-signer-hash "$ownerPubKeyHash" \
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

# Add a newline after the submission response.
echo ""
