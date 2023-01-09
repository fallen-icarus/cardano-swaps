# Variables
dir="../assets/plutus-files4/"
tmpDir="../assets/tmp/"
swapScriptFile="${dir}swap01.plutus"
swapScriptAddrFile="${dir}swap01.addr"
stakingScriptFile="${dir}staking01.plutus"
stakingScriptAddrFile="${dir}staking01.addr"
swapDatumFile="${dir}price.json"
beaconDatumFile="${dir}beaconDatum.json"
beaconRedeemerFile="${dir}beaconRedeemer.json"
beaconVaultScriptFile="${dir}beaconVault.plutus"
beaconVaultScriptAddrFile="${dir}beaconVault.addr"
beaconPolicyFile="${dir}beaconPolicy.plutus"

# Create the personal swap script file
cardano-swaps swap-script create-script \
  --owner-payment-key-hash $(cat ../assets/wallets/01.pkh) \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $swapScriptFile

# Create the staking script file
cardano-swaps staking-script create-script \
  --owner-payment-key-hash $(cat ../assets/wallets/01.pkh) \
  --out-file $stakingScriptFile

# Create the swap address with staking capabilities
cardano-cli address build \
  --payment-script-file $swapScriptFile \
  --stake-script-file $stakingScriptFile \
  --testnet-magic 1 \
  --out-file $swapScriptAddrFile

# Create the stake address
cardano-cli stake-address build \
  --stake-script-file $stakingScriptFile \
  --testnet-magic 1 \
  --out-file $stakingScriptAddrFile

# Create the desired Price datum
cardano-swaps swap-script create-datum \
  --swap-price 1.5 \
  --out-file $swapDatumFile

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

# Get the beacon vault address
cardano-swaps beacon vault-script --out-file $beaconVaultScriptFile

cardano-cli address build \
  --payment-script-file $beaconVaultScriptFile \
  --testnet-magic 1 \
  --out-file $beaconVaultScriptAddrFile

# Create the datum for the beacon vault deposit
cardano-swaps beacon create-datum --out-file $beaconDatumFile

# Create beacon redeemer
cardano-swaps beacon create-redeemer \
  --mint-beacon $beaconTokenName \
  --out-file $beaconRedeemerFile

# Get the beacon policy script
cardano-swaps beacon policy-script --out-file $beaconPolicyFile

# Deposit offered asset into swap address
# Save reference script at swap address in same utxo as beacon
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in aab3e03a7e1b3f9b462f1887d511e58c2dfafd65eb4b4a79834b967c0af98ce5#1 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 22000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 150000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ${beaconVaultScriptAddrFile}) + 2000000 lovelace" \
  --tx-out-inline-datum-file $beaconDatumFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral a4ccc449681f7e99869def7a88807d6f5064ae1f8e5e4178003b40b6cb9852fc#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"