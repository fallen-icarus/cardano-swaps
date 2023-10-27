alwaysSucceedSymbol="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
tokenName1=$(echo -n "TestToken1" | xxd -ps)
tokenName2=$(echo -n "TestToken2" | xxd -ps)
tokenName3=$(echo -n "TestToken3" | xxd -ps)
tokenName4=$(echo -n "TestToken4" | xxd -ps)
tmpDir="../../../ignored/tmp/"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in f0ff75deb465f9bfb00b164329462297afba04243ed56a72e77430ef86c0e24a#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName2}" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName3}" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName4}" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1} + 1000 ${alwaysSucceedSymbol}.${tokenName2} + 1000 ${alwaysSucceedSymbol}.${tokenName3} + 1000 ${alwaysSucceedSymbol}.${tokenName4}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address $(cat ../../../ignored/wallets/01.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
