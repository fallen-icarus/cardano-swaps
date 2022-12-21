# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
stakingScriptFile="${dir}staking.plutus"
stakingScriptAddrFile="${dir}staking.addr"
stakingRedeemerFile="${dir}stakingRedeemer.json"

# Create the staking redeemer
cabal run -v0 cardano-swaps -- create-staking-redeemer \
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
  --tx-in b3eebf98d8edeabd0d1813da1e8aa6d95d557d99dce9a9a2cbdbb1c1e72750b3#2 \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-file "${tmpDir}delegation.cert" \
  --certificate-script-file $stakingScriptFile \
  --certificate-redeemer-file $stakingRedeemerFile \
  --required-signer-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
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