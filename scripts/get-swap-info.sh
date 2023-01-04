# Variables
dir="../assets/plutus-files2/"
infoRedeemerFile="${dir}info.json"
tmpDir="../assets/tmp/"

# Create Info redeemer file
cardano-swaps swap-script create-redeemer \
  --owner-info \
  --out-file $infoRedeemerFile

# Execute the script - no need to sign and submit
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-tx-in-reference 325f5c8028f867c3dfdcacf750cab0fb43b2ad82d8d606c5b94142a5eb4fd58f#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $infoRedeemerFile \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
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