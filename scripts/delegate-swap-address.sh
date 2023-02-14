# Variables
tmpDir="../assets/tmp/"


# Create the registration certificate
cardano-cli stake-address registration-certificate \
  --stake-verification-key-file ../assets/wallets/01Stake.vkey \
  --out-file "${tmpDir}registration.cert"

# Create the delegation certificate
cardano-cli stake-address delegation-certificate \
  --stake-verification-key-file ../assets/wallets/01Stake.vkey \
  --stake-pool-id pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa \
  --out-file "${tmpDir}delegation.cert"

# Create and submit the transaction
# The transaction fee and deposit CANNOT come from the swap address. The owner must
# pay them from his/her personal address.
cardano-cli transaction build \
  --tx-in ef9c4009afa2eed41e4cf231e738062c114198d528f5efa78c0c2e016c14c119#0 \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-file "${tmpDir}delegation.cert" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "../assets/wallets/01.skey" \
  --signing-key-file "../assets/wallets/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"