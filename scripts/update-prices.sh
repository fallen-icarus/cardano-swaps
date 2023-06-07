# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"

swapAddrFile="${dir}swap.addr"

swapDatumFile="${dir}datum.json"

swapRedeemerFile="${dir}update.json"

# Export the spending script for that trading pair.
cardano-swaps export-script swap-script \
  --offered-asset-is-lovelace \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $spendingScriptFile

# Create the Update redeemer file.
cardano-swaps swap-redeemer \
  --update \
  --out-file $swapRedeemerFile

# Create the new datum for the positions.
cardano-swaps datum swap-datum \
  --price-numerator 1 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#26 \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#1 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#2 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#3 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#4 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#5 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#6 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#7 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#8 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#9 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#10 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#11 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#12 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#13 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#14 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#15 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#16 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --required-signer-hash $(cat ../assets/wallets/01Stake.pkh) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --signing-key-file ../assets/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"