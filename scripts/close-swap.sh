# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"
beaconPolicyFile="${dir}beacon.plutus"

swapAddrFile="${dir}swap.addr"

swapRedeemerFile="${dir}close.json"
beaconRedeemerFile="${dir}burn.json"

# Optional: Export the spending script for that trading pair.
# cardano-swaps swaps export-script \
#   --offered-asset-is-ada \
#   --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --asked-asset-token-name 4f74686572546f6b656e0a \
#   --out-file $spendingScriptFile

# Export the beacon policy for that trading pair.
cardano-swaps beacons export-policy \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $beaconPolicyFile

# Create the Close redeemer file.
cardano-swaps swaps create-redeemer \
  --close \
  --out-file $swapRedeemerFile

# Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}."

# Create the beacon redeemer to burn the beacon.
cardano-swaps beacons create-redeemer \
  --burn-beacon \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 192cfb283919692edc155d5f9e6468d0ad0abc32f23c1a1c821f37c2b38feb3f#0 \
  --spending-tx-in-reference 192cfb283919692edc155d5f9e6468d0ad0abc32f23c1a1c821f37c2b38feb3f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --mint "-1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
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