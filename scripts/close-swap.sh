# Variables
dir="../assets/plutus-files2/"
swapScriptFile="${dir}swap01.plutus"
closeRedeemerFile="${dir}close.json"
beaconRedeemer="${dir}beaconRedeemer.json"
tmpDir="../assets/tmp/"
beaconPolicyFile="${dir}beaconPolicy.plutus"
beaconVaultScriptFile="${dir}beaconVault.plutus"

# Create Close redeemer file
cardano-swaps swap-script create-redeemer \
  --close-swap \
  --out-file $closeRedeemerFile

# Get the beacon policy id
beaconPolicyId=$(cardano-swaps beacon policy-id --stdout)

# Generate the beacon token name
beaconTokenName=$(cardano-swaps beacon generate-token-name \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --stdout)

# Helper beacon variable
beacon="${beaconPolicyId}.${beaconTokenName}"

# Get the beacon policy script
cardano-swaps beacon policy-script --out-file $beaconPolicyFile

# Get the beacon vault script
cardano-swaps beacon vault-script --out-file $beaconVaultScriptFile

# Create beacon redeemer
cardano-swaps beacon create-redeemer \
  --burn-beacon $beaconTokenName \
  --out-file $beaconRedeemer

# Close the swap
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#3 \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-tx-in-reference 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#1 \
  --spending-tx-in-reference 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#2 \
  --tx-in-script-file $beaconVaultScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $beaconRedeemer \
  --tx-in-collateral a4ccc449681f7e99869def7a88807d6f5064ae1f8e5e4178003b40b6cb9852fc#0 \
  --mint "-1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemer \
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