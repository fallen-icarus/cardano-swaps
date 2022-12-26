# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
swapScriptFile="${dir}swap03.plutus"
swapScriptAddrFile="${dir}swap03.addr"
stakingScriptFile="${dir}staking03.plutus"
stakingScriptAddrFile="${dir}staking03.addr"
swapDatumFile="${dir}price.json"
beaconDatumFile="${dir}beaconDatum.json"
beaconRedeemerFile="${dir}beaconRedeemer.json"
beaconVaultScriptFile="${dir}beaconVaultScript.plutus"
beaconVaultScriptAddrFile="${dir}beaconVaultScript.addr"

beaconPolicyFile="${dir}beaconPolicy.plutus"

# Create the swap script file
cabal run cardano-swaps -- swap-script create-script \
  --owner-payment-key-hash $(cat ../assets/wallets/03.pkh) \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $swapScriptFile

# Create the staking script file
cabal run -v0 cardano-swaps -- staking-script create-script \
  --owner-payment-key-hash $(cat ../assets/wallets/03.pkh) \
  --out-file $stakingScriptFile

# Create the script address with staking capabilities
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
cabal run -v0 cardano-swaps -- swap-script create-datum \
  --swap-price 1 \
  --out-file $swapDatumFile

# Get the beacon policy id
beaconPolicyId=$(cabal run -v0 cardano-swaps -- beacon policy-id --stdout)

# Generate the beacon token name
beaconTokenName=$(cabal run -v0 cardano-swaps -- beacon generate-token-name \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --stdout)

# Helper beacon variable
beacon="${beaconPolicyId}.${beaconTokenName}"

# Get the beacon vault address
cabal run -v0 cardano-swaps -- beacon vault-script --out-file $beaconVaultScriptFile

cardano-cli address build \
  --payment-script-file $beaconVaultScriptFile \
  --testnet-magic 1 \
  --out-file $beaconVaultScriptAddrFile

# Create the datum for the beacon vault deposit
cabal run -v0 cardano-swaps -- beacon create-datum --out-file $beaconDatumFile

# Create beacon redeemer
cabal run -v0 cardano-swaps -- beacon create-redeemer \
  --mint-beacon $beaconTokenName \
  --out-file $beaconRedeemerFile

# Deposit offered asset into swap address
# Save reference script at swap address in same utxo as beacon
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 00267b3dafac0bbc886b44377a33bb2a2d131526668b1d1e31db6279094e1d7e#1 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 24000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 150000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ${beaconVaultScriptAddrFile}) + 2000000 lovelace" \
  --tx-out-inline-datum-file $beaconDatumFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/03.addr) \
  --tx-in-collateral 00267b3dafac0bbc886b44377a33bb2a2d131526668b1d1e31db6279094e1d7e#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/03.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"