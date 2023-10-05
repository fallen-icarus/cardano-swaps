#!/bin/sh

# Any beacons not re-output to the swap address must be burned.

# The following example closes one swap and updates the asking price for another swap.

# Variables
dir="../../ignored/swap-files/"
tmpDir="../../ignored/tmp/"

ownerPubKeyFile="../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}swap.addr"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}closeOrUpdate.json"

beaconRedeemerFile1="${dir}beaconRedeemer1.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the CloseOrUpdate redeemer.
echo "Creating the spending redeemer..."
cardano-swaps swap-redeemer \
  --close-or-update \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps swap-datum \
  --offer-lovelace \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --price-numerator 20 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile1

# Helper beacon variables.
beaconPolicyId1=$(cardano-swaps beacon-info policy-id \
  --offer-lovelace \
  --stdout)

beaconName1=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --stdout)

beaconName2=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 54657374546f6b656e34 \
  --stdout)

beacon1="${beaconPolicyId1}.${beaconName1}"
beacon2="${beaconPolicyId1}.${beaconName2}"

# Creating the beacon policy redeemer. If no beacons need to be minted, it is cheaper to use
# the BurnBeacons redeemer. 
echo "Creating the beacon policy redeemer..."
cardano-swaps beacon-redeemer burn \
  --out-file $beaconRedeemerFile1

# However, if even one beacon must be minted by that policy, the CreateSwap is required instead. In 
# this scenario, the beacons actually being minted/burned must be included in the redeemer. All 
# beacon policies must get their own redeemer. 
#
# cardano-swaps beacon-redeemer mint \
#   --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --ask-token-name 4f74686572546f6b656e0a \
#   --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --ask-token-name 54657374546f6b656e34 \
#   --out-file $beaconRedeemerFile1

# Create the transaction.
cardano-cli transaction build \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#2 \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#1 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --mint "-1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile1 \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address "$(cat ../../ignored/wallets/01.addr)" \
  --required-signer-hash "$ownerPubKeyHash" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../ignored/wallets/01.skey \
  --signing-key-file ../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
