# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"
beaconPolicyFile="${dir}beacon.plutus"

swapAddrFile="${dir}swap.addr"

swapRedeemerFile="${dir}close.json"
beaconRedeemerFile="${dir}burn.json"

# Export the spending script for that trading pair.
cardano-swaps export-script swap-script \
  --offered-asset-is-lovelace \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $spendingScriptFile

# Export the beacon policy for that trading pair.
cardano-swaps export-script beacon-policy \
  --offered-asset-is-lovelace \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $beaconPolicyFile

# Create the Close redeemer file.
cardano-swaps swap-redeemer \
  --close \
  --out-file $swapRedeemerFile

# Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}."

# Create the beacon redeemer to burn the beacon.
cardano-swaps beacon-redeemer \
  --burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#0 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#17 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#18 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#19 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#20 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#21 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#22 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#23 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#24 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 9f8700297c1e7ba433175dc2b85593ad93b9e12fd3cd8d55500d4e631e0eef9d#25 \
  --tx-in-script-file $spendingScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $swapRedeemerFile \
  --mint "-1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
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