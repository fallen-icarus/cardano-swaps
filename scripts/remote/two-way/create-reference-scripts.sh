#!/bin/sh

# The reference scripts may already be locked on-chain. Check the two-way swap address without a
# staking credential. Both the spending script and the beacon script will be permanently locked in
# this address.
#
# cardano-cli conway address build \
#   --payment-script-file $swapScriptFile \
#   --testnet-magic 1 \
#   --out-file $swapAddrFile
#
# Preprod: addr_test1wzrns8ct7stw9kh8f97nlnvqsl8kw7eukje2aw3kak8c77g25nluj
# Mainnet: addr1wxrns8ct7stw9kh8f97nlnvqsl8kw7eukje2aw3kak8c77g3u8rnh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapScriptFile="${tmpDir}twoWaySwap.plutus"
beaconPolicyFile="${tmpDir}twoWayBeacon.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

## Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts two-way beacon-script \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((8180352038))

echo "Building the initial transaction..."
cardano-cli conway build-raw \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#0 \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#1 \
  --tx-in 44f58115ad9738de64bb5624b495a2e7abddfdbe055df474d7d980af1244d64e#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + 24000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 22000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace " \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
req_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --witness-count 1 | cut -d' ' -f1)

echo "Rebuilding the transaction with the required fee..."
cardano-cli conway build-raw \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#0 \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#1 \
  --tx-in 44f58115ad9738de64bb5624b495a2e7abddfdbe055df474d7d980af1244d64e#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + 24000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 22000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
