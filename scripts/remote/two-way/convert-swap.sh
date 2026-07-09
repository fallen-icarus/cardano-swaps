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
beaconScriptSize=4804

spendingScriptPreprodTestnetRef="9415db73d8d374572a58ad167e3051110251aff802987f8627b27e060dcd673f#0"
spendingScriptSize=5007

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
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --first-price '1 / 1000000' \
  --second-price '3000000 / 1' \
  --expiration $expirationTime \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

oldPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

oldAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldAsset1Beacon="${beaconPolicyId}.${oldAsset1BeaconName}"
oldAsset2Beacon="${beaconPolicyId}.${oldAsset2BeaconName}"

newPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

newAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newAsset1Beacon="${beaconPolicyId}.${newAsset1BeaconName}"
newAsset2Beacon="${beaconPolicyId}.${newAsset2BeaconName}"

# Create the minting redeemer. This redeemer can also burn the old beacons.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((336576154))

echo "Building the initial transaction..."
cardano-cli conway transaction build-raw \
  --tx-in ac20e6b1cb9723b736f5360253ba3628d17051b7d20a5abb6b83e45b43065128#1 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#1 \
  --tx-in 84e649f06340eb887663dd5211259367b27638f842778b14382359983130bfb2#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v3 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) 21000000 lovelace" \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-hereafter $expirationSlot \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

# MAKE SURE THE INDEXES MATCH THE LEXICOGRAPHICAL ORDERING FOR INPUTS AND POLICY IDS.
# You can use `cardano-cli debug transaction view --tx-file "${tmpDir}tx.body` to view the prior
# transaction with everything in the correct order.
mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="mint" and .validator.index==0) | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="mint" and .validator.index==0) | .budget.cpu' )
spend_0_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="spend" and .validator.index==0) | .budget.memory' )
spend_0_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="spend" and .validator.index==0) | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli conway transaction build-raw \
  --tx-in ac20e6b1cb9723b736f5360253ba3628d17051b7d20a5abb6b83e45b43065128#1 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#1 \
  --tx-in 84e649f06340eb887663dd5211259367b27638f842778b14382359983130bfb2#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v3 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) 21000000 lovelace" \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-hereafter $expirationSlot \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --reference-script-size $((beaconScriptSize+spendingScriptSize)) \
  --witness-count 2 --output-json | jq .fee)
req_fee=$((calculated_fee+100000)) # Add 0.1 ADA to be safe since the fee must still be updated.
req_collateral=$(printf %.0f $(echo "${req_fee}*1.5" | bc))

echo "Rebuilding the transaction with the required fee..."
cardano-cli conway transaction build-raw \
  --tx-in ac20e6b1cb9723b736f5360253ba3628d17051b7d20a5abb6b83e45b43065128#1 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#1 \
  --tx-in 84e649f06340eb887663dd5211259367b27638f842778b14382359983130bfb2#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v3 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v3 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral $req_collateral \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) $((21000000-$req_collateral)) lovelace" \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-hereafter $expirationSlot \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --signing-key-file $HOME/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
