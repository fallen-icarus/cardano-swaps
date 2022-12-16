# Variables
dir="../assets/plutus-files/"
swapScriptFile="${dir}swap.plutus"
closeRedeemerFile="${dir}close.json"
tmpDir="../assets/tmp/"

# Create Close redeemer file
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --close-swap \
  --out-file $closeRedeemerFile

# Close the swap
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in da529c24388f04903cddf59849cde1e5645124fa1fe66414f341f48c37a9ea8d#2 \
  --tx-in da529c24388f04903cddf59849cde1e5645124fa1fe66414f341f48c37a9ea8d#0 \
  --spending-tx-in-reference da529c24388f04903cddf59849cde1e5645124fa1fe66414f341f48c37a9ea8d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in da529c24388f04903cddf59849cde1e5645124fa1fe66414f341f48c37a9ea8d#1 \
  --spending-tx-in-reference da529c24388f04903cddf59849cde1e5645124fa1fe66414f341f48c37a9ea8d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in-collateral bfc39666bcfa83a5ed1bbbb67de4b9a2bb33d3f9af03ab35fd7a4e6ac28e411b#0 \
  --tx-out "$(cat ../assets/wallets/01.addr) 5000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a" \
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