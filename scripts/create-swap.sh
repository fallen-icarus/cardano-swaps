# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
swapScriptFile="${dir}swap.plutus"
swapScriptAddrFile="${dir}swap.addr"
swapDatumFile="${dir}price.json"
stakingScriptFile="${dir}staking.plutus"
stakingScriptAddrFile="${dir}staking.addr"
beaconTokenNameFile="${dir}beaconTokenName.txt"
unitDatumFile="${dir}unit.json"
beaconRedeemer="${dir}beaconRedeemer.json"
# beaconTokenName="$(cat ${beaconTokenNameFile} | xxd -r)"
# beaconTokenNameHex="$(cat ${beaconTokenNameFile})"

beaconScriptFile="${dir}beaconScript.plutus"
beaconSymbol="$(cat ${dir}beaconSymbol.txt)"
beaconVaultScriptFile="${dir}beaconVaultScript.plutus"
beaconVaultScriptAddrFile="${dir}beaconVaultScript.addr"

beacon="${beaconSymbol}.1111"

# Create the swap script file
cabal run cardano-swaps -- create-swap-script \
  --owner-payment-key-hash $(cat ../assets/wallets/02.pkh) \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $swapScriptFile

# Create the staking script file (only based off owner)
cabal run -v0 cardano-swaps -- create-staking-script \
  --owner-payment-key-hash $(cat ../assets/wallets/02.pkh) \
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
cabal run -v0 cardano-swaps -- create-swap-datum \
  --swap-price 1.5 \
  --out-file $swapDatumFile

# # Generate the beacon token name
# cabal run -v0 cardano-swaps -- create-beacon-token-name \
#   --offered-asset-is-ada \
#   --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --asked-asset-token-name 4f74686572546f6b656e0a \
#   --out-file $beaconTokenNameFile

# Create datum for beacon vault deposit
cabal run -v0 cardano-swaps -- create-staking-redeemer \
  --out-file $unitDatumFile

# Create beacon redeemer
cabal run -v0 cardano-swaps -- create-beacon-redeemer \
  --mint-beacon 1111 \
  --out-file $beaconRedeemer

# Deposit offered asset into swap address
# Save reference script at swap address in same utxo as beacon
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 96baac9bb4189acf9b9cb49573e9c0a4a492344a4e4216fa7e90e48966434b10#1 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 23000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 500000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ${beaconVaultScriptAddrFile}) + 1000000 lovelace" \
  --tx-out-inline-datum-file $unitDatumFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconScriptFile \
  --mint-redeemer-file $beaconRedeemer \
  --change-address $(cat ../assets/wallets/02.addr) \
  --tx-in-collateral af3b8901a464f53cb69e6e240a506947154b1fedbe89ab7ff9263ed2263f5cf5#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"