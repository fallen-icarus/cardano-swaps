# Variables
dir="../assets/plutus-files2/"
swapRedeemerFile="${dir}swap.json"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap01.addr"
swapDatumFile="${dir}price.json"
swapScriptFile="${dir}swap01.plutus"

# Create Swap redeemer file
cardano-swaps swap-script create-redeemer \
  --swap-assets \
  --out-file $swapRedeemerFile

# Create the swap datum file
# cardano-swaps swap-script create-datum \
#   --calc-swap-price-from-file ../assets/plutus-files2/template.json \
#   --out-file $swapDatumFile

cardano-swaps swap-script create-datum \
  --swap-price 1 \
  --out-file $swapDatumFile

# Swap assets
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in bfc39666bcfa83a5ed1bbbb67de4b9a2bb33d3f9af03ab35fd7a4e6ac28e411b#0 \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#1 \
  --spending-tx-in-reference 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 150000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
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