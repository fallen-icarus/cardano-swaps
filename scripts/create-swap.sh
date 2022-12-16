# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
swapScriptFile="${dir}swap.plutus"
swapScriptAddrFile="${dir}swap.addr"
swapDatumFile="${dir}price.json"

# Create the script file
cabal run cardano-swaps -- create-swap-script \
  --owner-payment-key-hash $(cat ../assets/wallets/02.pkh) \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 54657374546f6b656e0a \
  --offered-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offered-asset-token-name 4f74686572546f6b656e0a \
  --out-file $swapScriptFile

# Create the script address
cardano-cli address build \
  --payment-script-file $swapScriptFile \
  --testnet-magic 1 \
  --out-file $swapScriptAddrFile

# Create the desired Price datum
cabal run -v0 cardano-swaps -- create-swap-datum \
  --swap-price 2 \
  --out-file $swapDatumFile

# Deposit offered asset into swap address
cardano-cli transaction build \
  --tx-in 5a1a2435ee319a21c867143ee4a56c1b9f12f9b86a3dc0d93a8e0257eeb5b917#0 \
  --tx-in 411a0aca4a9827de2a7ea7a609b12794040beb66df05a8af6cb77281688cff0b#0 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 23000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../assets/wallets/02.addr) 2000000 + 900 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ${swapScriptAddrFile}) + 2000000 lovelace + 100 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --change-address $(cat ../assets/wallets/02.addr) \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"