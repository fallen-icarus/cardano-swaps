# Variables
dir="../assets/plutus-files/"
swapScriptFile="${dir}swap.plutus"
infoRedeemerFile="${dir}info.json"
tmpDir="../assets/tmp/"

# Create Info redeemer file
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --owner-info \
  --out-file $infoRedeemerFile

# Execute the script - no need to sign and submit
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#0 \
  --spending-tx-in-reference 622034715b64318e9e2176b7ad9bb22c3432f360293e9258729ce23c1999b9d8#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $infoRedeemerFile \
  --tx-in-collateral af3b8901a464f53cb69e6e240a506947154b1fedbe89ab7ff9263ed2263f5cf5#0 \
  --change-address $(cat ../assets/wallets/02.addr) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

# Example Output:
#
# Command failed: transaction build  Error: The following scripts have execution failures:
# the script for transaction input 0 (in the order of the TxIds) failed with: 
# The Plutus script evaluation failed: An error has occurred:  User error:
# The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
# Caused by: [
#   (builtin decodeUtf8)
#   (con bytestring #fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f)
# ]
# Script debugging logs: 
#
# fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f is the owner's pubkey hash