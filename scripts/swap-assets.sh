# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"

swapAddrFile="${dir}swap.addr"

swapDatumFile="${dir}datum.json"

swapRedeemerFile="${dir}swap.json"

# Optional: Export the spending script for that trading pair.
# cardano-swaps swaps export-script \
#   --offered-asset-is-ada \
#   --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --asked-asset-token-name 4f74686572546f6b656e0a \
#   --out-file $spendingScriptFile

# Create the Swap redeemer file.
cardano-swaps swaps create-redeemer \
  --swap \
  --out-file $swapRedeemerFile

# Create the new datum for the outputs at the swap address.
cardano-swaps swaps create-datum \
  --swap-price 2 \
  --out-file $swapDatumFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in c65b26c54408314f9f69e29df50ad70ef8cb1c6cb67c2c9847ce82bae28e7148#2 \
  --tx-in c65b26c54408314f9f69e29df50ad70ef8cb1c6cb67c2c9847ce82bae28e7148#1 \
  --tx-in d162ff8dd6bbf4e5673145d6c97334771b0b72f2ed77e60f618044a09124df80#0 \
  --spending-tx-in-reference b9fefb2cbfaeaf687bf0cf2553220093b43be5877c583574f77fefd847bc3a80#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in d162ff8dd6bbf4e5673145d6c97334771b0b72f2ed77e60f618044a09124df80#1 \
  --spending-tx-in-reference b9fefb2cbfaeaf687bf0cf2553220093b43be5877c583574f77fefd847bc3a80#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in d162ff8dd6bbf4e5673145d6c97334771b0b72f2ed77e60f618044a09124df80#2 \
  --spending-tx-in-reference b9fefb2cbfaeaf687bf0cf2553220093b43be5877c583574f77fefd847bc3a80#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 30000000 lovelace + 0 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 + 500 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
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