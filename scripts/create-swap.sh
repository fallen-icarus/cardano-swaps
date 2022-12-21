# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
swapScriptFile="${dir}swap(Tok2-ADA).plutus"
swapScriptAddrFile="${dir}swap(Tok2-ADA).addr"
swapDatumFile="${dir}price.json"
stakingScriptFile="${dir}staking.plutus"
stakingScriptAddrFile="${dir}staking.addr"

# Create the swap script file
cabal run cardano-swaps -- create-swap-script \
  --owner-payment-key-hash $(cat ../assets/wallets/01.pkh) \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $swapScriptFile

# Create the staking script file (only based off owner)
cabal run -v0 cardano-swaps -- create-staking-script \
  --owner-payment-key-hash $(cat ../assets/wallets/01.pkh) \
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

# Deposit offered asset into swap address
cardano-cli transaction build \
  --tx-in 59406a3b820d6018b7059974f8451c80f9606d60433b4642b112276412200e34#1 \
  --tx-out "$(cat ${swapScriptAddrFile}) + 23000000 lovelace" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ${swapScriptAddrFile}) + 500000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --change-address $(cat ../assets/wallets/01.addr) \
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