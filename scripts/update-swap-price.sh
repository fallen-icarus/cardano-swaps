# Variables
dir="../assets/plutus-files2/"
updateRedeemerFile="${dir}update.json"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap01.addr"
swapDatumFile="${dir}price.json"

# Create Update redeemer file
cardano-swaps swap-script create-redeemer \
  --update-swap-price 1.5 \
  --out-file $updateRedeemerFile

# Create the datums for the newly created utxos
cardano-swaps swap-script create-datum \
  --swap-price 1.5 \
  --out-file $swapDatumFile

# Update the swap price
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in bfc39666bcfa83a5ed1bbbb67de4b9a2bb33d3f9af03ab35fd7a4e6ac28e411b#0 \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#1 \
  --spending-tx-in-reference 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $updateRedeemerFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 150000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral a4ccc449681f7e99869def7a88807d6f5064ae1f8e5e4178003b40b6cb9852fc#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --required-signer-hash $(cat ../assets/wallets/01.pkh) \
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