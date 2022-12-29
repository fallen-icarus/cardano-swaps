# Variables
dir="../assets/plutus-files2/"
tmpDir="../assets/tmp/"
stakingScriptFile="${dir}staking01.plutus"
stakingScriptAddrFile="${dir}staking01.addr"
stakingRedeemerFile="${dir}stakingRedeemer.json"

# Create the staking redeemer
cardano-swaps staking-script create-redeemer \
  --out-file $stakingRedeemerFile

# Create the registration certificate
cardano-cli stake-address registration-certificate \
  --stake-script-file $stakingScriptFile \
  --out-file "${tmpDir}registration.cert"

# Create the delegation certificate
cardano-cli stake-address delegation-certificate \
  --stake-script-file $stakingScriptFile \
  --stake-pool-id pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa \
  --out-file "${tmpDir}delegation.cert"

# Get the protocol parameters
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

# Create and submit the transaction
# The transaction fee and deposit CANNOT come from the swap address. The owner must
# pay them from his/her personal address.
cardano-cli transaction build \
  --tx-in bfc39666bcfa83a5ed1bbbb67de4b9a2bb33d3f9af03ab35fd7a4e6ac28e411b#0 \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-file "${tmpDir}delegation.cert" \
  --certificate-script-file $stakingScriptFile \
  --certificate-redeemer-file $stakingRedeemerFile \
  --required-signer-hash $(cat ../assets/wallets/01.pkh) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "../assets/wallets/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"