# Variables
dir="../assets/plutus-files/"
swapRedeemerFile="${dir}swap.json"
tmpDir="../assets/tmp/"

swap1ScriptAddrFile="${dir}swap(ADA-Tok1).addr"
swap1DatumFile="${dir}price(ADA-Tok1).json"
swap1ScriptFile="${dir}swap(ADA-Tok1).plutus"

swap2ScriptAddrFile="${dir}swap(Tok1-Tok2).addr"
swap2DatumFile="${dir}price(Tok1-Tok2).json"
swap2ScriptFile="${dir}swap(Tok1-Tok2).plutus"

swap3ScriptAddrFile="${dir}swap(Tok2-ADA).addr"
swap3DatumFile="${dir}price(Tok2-ADA).json"
swap3ScriptFile="${dir}swap(Tok2-ADA).plutus"

# Create Swap redeemer file
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --swap-assets \
  --out-file $swapRedeemerFile

# Create all Price datum files for swap change
cabal run -v0 cardano-swaps -- create-swap-datum \
  --calc-swap-price-from-file "${dir}utxos(ADA-Tok1).json" \
  --out-file $swap1DatumFile

# cabal run -v0 cardano-swaps -- create-swap-datum \
#   --swap-price 1 \
#   --out-file $swap1DatumFile

cabal run -v0 cardano-swaps -- create-swap-datum \
  --swap-price 1 \
  --out-file $swap2DatumFile

cabal run -v0 cardano-swaps -- create-swap-datum \
  --calc-swap-price-from-file "${dir}utxos(Tok2-ADA).json" \
  --out-file $swap3DatumFile

# cabal run -v0 cardano-swaps -- create-swap-datum \
#   --swap-price 0.5 \
#   --out-file $swap3DatumFile

# Chain Swaps
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 00267b3dafac0bbc886b44377a33bb2a2d131526668b1d1e31db6279094e1d7e#1 \
  --tx-in 9cedc917d30cdec70242bb6f48ec9e5d59a4c03b4fc95518d42beb8636da35fb#1 \
  --spending-tx-in-reference 9cedc917d30cdec70242bb6f48ec9e5d59a4c03b4fc95518d42beb8636da35fb#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 72519dc118232d7979f320e9ae120bcb6b22a7c31d4091aea886f761b6a982ce#0 \
  --spending-tx-in-reference 9cedc917d30cdec70242bb6f48ec9e5d59a4c03b4fc95518d42beb8636da35fb#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 881242ed8483ee8fefc09f0a63f640c0959df00dc56a685b05468c5356b2111b#0 \
  --spending-tx-in-reference b82002ac3bc334ab5ff410fea87f6be2ca80b2f915b6169d895b2a0186cf90c0#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in b82002ac3bc334ab5ff410fea87f6be2ca80b2f915b6169d895b2a0186cf90c0#1 \
  --spending-tx-in-reference b82002ac3bc334ab5ff410fea87f6be2ca80b2f915b6169d895b2a0186cf90c0#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 0578f030b834d7a8a534e3a5fa0dd1bdc0ab0f1f4dbf7f42046dc76a0e6085d4#1 \
  --spending-tx-in-reference 0578f030b834d7a8a534e3a5fa0dd1bdc0ab0f1f4dbf7f42046dc76a0e6085d4#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 59406a3b820d6018b7059974f8451c80f9606d60433b4642b112276412200e34#0 \
  --spending-tx-in-reference 0578f030b834d7a8a534e3a5fa0dd1bdc0ab0f1f4dbf7f42046dc76a0e6085d4#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../assets/wallets/03.addr) 2000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 280 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ${swap1ScriptAddrFile}) + 829000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swap1DatumFile \
  --tx-out "$(cat ${swap2ScriptAddrFile}) + 4000000 lovelace + 650 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 350 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swap2DatumFile \
  --tx-out "$(cat ${swap3ScriptAddrFile}) + 2000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e0a + 370 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swap3DatumFile \
  --tx-in-collateral 00267b3dafac0bbc886b44377a33bb2a2d131526668b1d1e31db6279094e1d7e#0 \
  --change-address $(cat ../assets/wallets/03.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"