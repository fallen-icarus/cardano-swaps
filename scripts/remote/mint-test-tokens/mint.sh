alwaysSucceedSymbol="c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"
tokenName1=$(echo -n "TestToken1" | xxd -ps)
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((19759481-2000000))

echo "Building the initial transaction..."
cardano-cli conway transaction build-raw \
  --tx-in b6b5bd23fa762b2630dc9dedc10d0bac61d6ffa3617f451df8a8ee31a83c441f#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat $HOME/wallets/01.addr) ${initial_change} lovelace" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --mint-execution-units "(0,0)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-total-collateral 21000000 \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) 21000000 lovelace" \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="mint" and .validator.index==0) | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="mint" and .validator.index==0) | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli conway transaction build-raw \
  --tx-in b6b5bd23fa762b2630dc9dedc10d0bac61d6ffa3617f451df8a8ee31a83c441f#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat $HOME/wallets/01.addr) ${initial_change} lovelace" \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --mint-execution-units "(${mint_steps},${mint_mem})" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-total-collateral 21000000 \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) 21000000 lovelace" \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.
req_collateral=$(printf %.0f $(echo "${req_fee}*1.5" | bc))

echo "Rebuilding the transaction with required transaction fee..."
cardano-cli conway transaction build-raw \
  --tx-in b6b5bd23fa762b2630dc9dedc10d0bac61d6ffa3617f451df8a8ee31a83c441f#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) 2000000 lovelace + 1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --tx-out "$(cat $HOME/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "1000 ${alwaysSucceedSymbol}.${tokenName1}" \
  --mint-script-file alwaysSucceedsMintingPolicy.plutus \
  --mint-redeemer-file unit.json \
  --mint-execution-units "(${mint_steps},${mint_mem})" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-total-collateral $req_collateral \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) $((21000000-$req_collateral)) lovelace" \
  --fee $req_fee \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
