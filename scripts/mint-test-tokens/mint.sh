alwaysSucceedSymbol="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
tokenName=$(echo -n "OtherToken" | xxd -ps)
tmpDir="../../assets/tmp/"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 2d3bad47abe57524ad627bb11a9ed0ff22f06538b6ba6794834155615f6c212a#0 \
  --tx-out "$(cat ../../assets/wallets/02.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName}" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --tx-in-collateral af3b8901a464f53cb69e6e240a506947154b1fedbe89ab7ff9263ed2263f5cf5#0 \
  --change-address $(cat ../../assets/wallets/02.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"