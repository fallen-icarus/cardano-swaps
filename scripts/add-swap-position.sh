# Variables
dir="../assets/plutus-files2/"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap01.addr"
swapDatumFile="${dir}price.json"

# Create the desired Price datum
cardano-swaps swap-script create-datum \
  --swap-price 2.75 \
  --out-file $swapDatumFile

# Deposit offered asset into swap address
cardano-cli transaction build \
  --tx-in bfc39666bcfa83a5ed1bbbb67de4b9a2bb33d3f9af03ab35fd7a4e6ac28e411b#0 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
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