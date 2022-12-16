# Variables
dir="../assets/plutus-files/"
updateRedeemerFile="${dir}update.json"
tmpDir="../assets/tmp/"
swapScriptAddrFile="${dir}swap.addr"
swapDatumFile="${dir}price.json"
swapScriptFile="${dir}swap.plutus"

# Create Update redeemer file and new Price datum file
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --update-swap-price 1.5 \
  --out-file $updateRedeemerFile

cabal run -v0 cardano-swaps -- create-swap-datum \
  --swap-price 1.5 \
  --out-file $swapDatumFile

# Update the swap price
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 294071a64a9ddb540de29880747f148f2b857a7908049f68864162b39c42aaf2#2 \
  --tx-in 294071a64a9ddb540de29880747f148f2b857a7908049f68864162b39c42aaf2#1 \
  --spending-tx-in-reference 294071a64a9ddb540de29880747f148f2b857a7908049f68864162b39c42aaf2#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $updateRedeemerFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 2000000 lovelace + 1000 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral 4805706e4ae9bb27de8cb241137e81082f2e23e4be5e43d2eed63a3e24ac400f#0 \
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