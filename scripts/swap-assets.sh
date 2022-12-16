# Variables
dir="../assets/plutus-files/"
swapRedeemerFile="${dir}swap.json"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap.addr"
swapDatumFile="${dir}price.json"
swapScriptFile="${dir}swap.plutus"

# Create Swap redeemer file and Price datum file if necessary
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --swap-assets \
  --out-file $swapRedeemerFile

cabal run -v0 cardano-swaps -- create-swap-datum \
  --calc-swap-price-from-file ../assets/plutus-files/template.json \
  --out-file $swapDatumFile

# cabal run -v0 cardano-swaps -- create-swap-datum \
#   --swap-price 2 \
#   --out-file $swapDatumFile

# Swap assets
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in c1d01ea50fd233f9fbaef3a295ba607a72c736e58c9c9df588abf4e5009ad4fe#0 \
  --tx-in 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#2 \
  --spending-tx-in-reference 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 766555130db8ff7b50fc548cbff3caa0d0557ce5af804da3b993cd090f1a8c3a#1 \
  --spending-tx-in-reference 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../assets/wallets/01.addr) 2000000 lovelace + 300 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ${swapScriptAddrFile}) + 4000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 250 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

# cardano-cli transaction sign \
#   --tx-body-file "${tmpDir}tx.body" \
#   --signing-key-file ../assets/wallets/02.skey \
#   --testnet-magic 1 \
#   --out-file "${tmpDir}tx.signed"

# cardano-cli transaction submit \
#   --testnet-magic 1 \
#   --tx-file "${tmpDir}tx.signed"