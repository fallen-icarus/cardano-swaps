# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"

swapAddrFile="${dir}swap.addr"

swapDatumFile="${dir}datum.json"

swapRedeemerFile="${dir}swap.json"

# Export the spending script for that trading pair.
cardano-swaps export-script swap-script \
  --offered-asset-is-lovelace \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $spendingScriptFile

# Create the Swap redeemer file.
cardano-swaps swap-redeemer \
  --swap \
  --out-file $swapRedeemerFile

# Create the new datum for the outputs at the swap address.
cardano-swaps datum swap-datum \
  --price-numerator 1 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 9c0df175a90cc036d9a8916a3107fc2f8fa541cbbc01cbc6307d2b2b92623661#0 \
  --tx-in f962b4671bf73c4fcaf13cdb8fd45aa13a63852305bcb5327c217f879909607e#1 \
  --tx-in 6fc8120a9af641709643f4c51fca4f458b0faf2a201fe93c16713be3c6c6e96b#0 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 5000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 6fc8120a9af641709643f4c51fca4f458b0faf2a201fe93c16713be3c6c6e96b#1 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 5000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 6fc8120a9af641709643f4c51fca4f458b0faf2a201fe93c16713be3c6c6e96b#2 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 5000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 6fc8120a9af641709643f4c51fca4f458b0faf2a201fe93c16713be3c6c6e96b#3 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 5000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 6fc8120a9af641709643f4c51fca4f458b0faf2a201fe93c16713be3c6c6e96b#4 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 5000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 3000000 + 515 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
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