#!/bin/sh

# Any beacons not re-output to the swap address must be burned.

# The following example closes one swap and updates the asking price for another swap.

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}two_way_swap.addr"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}closeOrUpdateTwoWaySwap.json"

beaconRedeemerFile="${dir}twoWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the CloseOrUpdate redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --close-or-update \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --forward-price-numerator 10 \
  --forward-price-denominator 1000000 \
  --reverse-price-numerator 10 \
  --reverse-price-denominator 1000000 \
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

asset1BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Creating the beacon policy redeemer. If no beacons need to be minted, it is cheaper to use
# the BurnBeacons redeemer. 
echo "Creating the beacon policy redeemer..."
cardano-swaps beacon-redeemers two-way \
  --burn \
  --out-file $beaconRedeemerFile

# However, if even one beacon must be minted by that policy, the CreateSwap is required instead. In 
# this scenario, the beacons actually being minted/burned must be included in the redeemer. All 
# beacon policies must get their own redeemer. 
#
# cardano-swaps beacon-redeemers two-way \
#   --create-swap \
#   --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli transaction build \
  --tx-in 93a46aff123477ec8684f4e1f2815f3314f36f9547d5b05cac990ea1d95bc211#0 \
  --spending-tx-in-reference 4bc4daeec4044e3a022ac3bad9a19a0ac0d4ac9e3cabfc394716ffa17a8fc52f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 93a46aff123477ec8684f4e1f2815f3314f36f9547d5b05cac990ea1d95bc211#1 \
  --spending-tx-in-reference 4bc4daeec4044e3a022ac3bad9a19a0ac0d4ac9e3cabfc394716ffa17a8fc52f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1}" \
  --mint-tx-in-reference 4bc4daeec4044e3a022ac3bad9a19a0ac0d4ac9e3cabfc394716ffa17a8fc52f#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address "$(cat ../../../ignored/wallets/01.addr)" \
  --required-signer-hash "$ownerPubKeyHash" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --signing-key-file ../../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
