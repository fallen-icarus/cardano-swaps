# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap.addr"
swapDatumFile="${dir}price.json"

# Create the desired Price datum
cabal run -v0 cardano-swaps -- create-swap-datum \
  --swap-price 1.5 \
  --out-file $swapDatumFile

# Deposit offered asset into swap address
cardano-cli transaction build \
  --tx-in 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#1 \
  --tx-in 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#3 \
  --tx-out "$(cat ../assets/wallets/02.addr) + 5000000 lovelace + 750 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ${swapScriptAddrFile}) + 2000000 lovelace + 150 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
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