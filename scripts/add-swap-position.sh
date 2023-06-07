#!/bin/sh

# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

swapAddrFile="${dir}swap.addr"

swapDatumFile="${dir}datum.json"

# Create the new datum for the outputs at the swap address.
cardano-swaps datum swap-datum \
  --price-numerator 2 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile

# Create the transaction.
cardano-cli transaction build \
  --tx-in ef9c4009afa2eed41e4cf231e738062c114198d528f5efa78c0c2e016c14c119#0 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"