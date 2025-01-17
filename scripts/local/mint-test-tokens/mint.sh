alwaysSucceedSymbol="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
tokenName1=$(echo -n "TestToken1" | xxd -ps)
tokenName2=$(echo -n "TestToken2" | xxd -ps)
tokenName3=$(echo -n "TestToken3" | xxd -ps)
tokenName4=$(echo -n "TestToken4" | xxd -ps)
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

cardano-cli conway transaction build \
  --tx-in b9ede1a18cac5e771fcdcf9012f0005ee9f8baca7d4210a89d796646a52ee586#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1} + 1000 ${alwaysSucceedSymbol}.${tokenName2} + 1000 ${alwaysSucceedSymbol}.${tokenName3} + 1000 ${alwaysSucceedSymbol}.${tokenName4}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --change-address $(cat $HOME/wallets/01.addr) \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
