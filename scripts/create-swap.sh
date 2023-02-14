# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"
beaconPolicyFile="${dir}beacon.plutus"

swapAddrFile="${dir}swap.addr"

swapBeaconDatumFile="${dir}beaconDatum.json"
swapPositionDatumFile="${dir}positionDatum.json"

beaconRedeemerFile="${dir}mint.json"

# Export the spending script for that trading pair.
cardano-swaps swaps export-script \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $spendingScriptFile

# Export the beacon policy for that trading pair.
cardano-swaps beacons export-policy \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $beaconPolicyFile

# Create the swap address.
cardano-cli address build \
  --payment-script-file $spendingScriptFile \
  --stake-verification-key-file "../assets/wallets/01Stake.vkey" \
  --testnet-magic 1 \
  --out-file $swapAddrFile

# Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}."

# Create the datum for storing with the beacon
cardano-swaps beacons create-datum \
  --offered-asset-is-ada \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $swapBeaconDatumFile

# Create the datum for the first swap positions
cardano-swaps swaps create-datum \
  --swap-price 2 \
  --out-file $swapPositionDatumFile

# Create the beacon redeemer for minting the beacon.
cardano-swaps beacons create-redeemer \
  --mint-beacon \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 29701aaa3c70cdd300d3c4a80d388990f19a1010a774f59eaec10a55b4a39a02#0 \
  --tx-out "$(cat ${swapAddrFile}) + 23000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $swapBeaconDatumFile \
  --tx-out-reference-script-file $spendingScriptFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
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