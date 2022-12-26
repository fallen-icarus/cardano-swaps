# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap03.addr"
swapDatumFile="${dir}price.json"

# Create the desired Price datum
cabal run -v0 cardano-swaps -- swap-script create-datum \
  --swap-price 2.75 \
  --out-file $swapDatumFile

# Deposit offered asset into swap address
cardano-cli transaction build \
  --tx-in 96baac9bb4189acf9b9cb49573e9c0a4a492344a4e4216fa7e90e48966434b10#0 \
  --tx-in 225c7eeb878c874721ded13e6e4dd5d9bb89ec81868f5031c875d43c6963d64b#3 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 100000000 lovelace + 1000 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
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