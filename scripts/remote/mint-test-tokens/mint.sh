alwaysSucceedSymbol="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
tokenName1=$(echo -n "TestToken1" | xxd -ps)
# tokenName2=$(echo -n "TestToken2" | xxd -ps)
# tokenName3=$(echo -n "TestToken3" | xxd -ps)
# tokenName4=$(echo -n "TestToken4" | xxd -ps)
tmpDir="../../../ignored/tmp/"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((7550230497-2000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in e81920b63c8123093fb1245a033fec2ad0743c7c794ea57c257014f0a85b0ec3#3 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) ${initial_change} lovelace" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --mint-execution-units "(0,0)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-total-collateral 21000000 \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli transaction build-raw \
  --tx-in e81920b63c8123093fb1245a033fec2ad0743c7c794ea57c257014f0a85b0ec3#3 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) ${initial_change} lovelace" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --mint-execution-units "(${mint_steps},${mint_mem})" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-total-collateral 21000000 \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with required transaction fee..."
cardano-cli transaction build-raw \
  --tx-in e81920b63c8123093fb1245a033fec2ad0743c7c794ea57c257014f0a85b0ec3#3 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --mint-execution-units "(${mint_steps},${mint_mem})" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-total-collateral 21000000 \
  --fee $req_fee \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
