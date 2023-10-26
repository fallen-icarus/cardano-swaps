#!/bin/sh

# Any beacons not re-output to the swap address must be burned.

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}twoWaySwap.addr"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}closeOrUpdate.json"

beaconRedeemerFile="${dir}beaconRedeemer.json"

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
  --first-asset-lovelace \
  --second-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --second-token-name 4f74686572546f6b656e0a \
  --forward-price-numerator 1000000 \
  --forward-price-denominator 1 \
  --reverse-price-numerator 1 \
  --reverse-price-denominator 1000000 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset-lovelace \
  --second-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --second-token-name 4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --first-asset-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --second-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --second-token-name 4f74686572546f6b656e0a \
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
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((14202654))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 8420b1fab6862a0fb05496a5268c7ef840ac75fe623a323ca142991a799cf51c#0 \
  --tx-in f5e9cb3a3c2d41ba6ae15e62c9a8dafdb9a62e821acb7d8f328fe3429306b0b0#1 \
  --spending-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 5000000 lovelace + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

spend_0_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="spend:1") | .budget.memory' )
spend_0_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="spend:1") | .budget.cpu' )
mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in 8420b1fab6862a0fb05496a5268c7ef840ac75fe623a323ca142991a799cf51c#0 \
  --tx-in f5e9cb3a3c2d41ba6ae15e62c9a8dafdb9a62e821acb7d8f328fe3429306b0b0#1 \
  --spending-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 5000000 lovelace + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 2 \
  --tx-out-count 2 \
  --witness-count 2 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 8420b1fab6862a0fb05496a5268c7ef840ac75fe623a323ca142991a799cf51c#0 \
  --tx-in f5e9cb3a3c2d41ba6ae15e62c9a8dafdb9a62e821acb7d8f328fe3429306b0b0#1 \
  --spending-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 5000000 lovelace + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee $req_fee \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --signing-key-file ../../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
