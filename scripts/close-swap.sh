# Variables
dir="../assets/plutus-files/"
swapScriptFile="${dir}swap.plutus"
closeRedeemerFile="${dir}close.json"
beaconRedeemer="${dir}beaconRedeemer.json"
beaconTokenNameFile="${dir}beaconTokenName.txt"
tmpDir="../assets/tmp/"

beaconScriptFile="${dir}beaconScript.plutus"
beaconSymbol="$(cat ${dir}beaconSymbol.txt)"
beaconVaultScriptFile="${dir}beaconVaultScript.plutus"
beaconVaultScriptAddrFile="${dir}beaconVaultScript.addr"

beaconTokenName="$(cat ${beaconTokenNameFile})"
beacon="${beaconSymbol}.ce48dd16f80225ea3c8d74de877b93d333e69e2d1d39ede3287e25685a09f483"

# Create Close redeemer file
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --close-swap \
  --out-file $closeRedeemerFile

# Create beacon redeemer
cabal run -v0 cardano-swaps -- create-beacon-redeemer \
  --burn-beacon $beaconTokenName \
  --out-file $beaconRedeemer

# Close the swap
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 8553bceb870c7954177eebe6528adf99ab47e84fd276078fce2eef03e2fab5b4#3 \
  --tx-in 8553bceb870c7954177eebe6528adf99ab47e84fd276078fce2eef03e2fab5b4#0 \
  --spending-tx-in-reference 8553bceb870c7954177eebe6528adf99ab47e84fd276078fce2eef03e2fab5b4#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in 8553bceb870c7954177eebe6528adf99ab47e84fd276078fce2eef03e2fab5b4#1 \
  --spending-tx-in-reference 8553bceb870c7954177eebe6528adf99ab47e84fd276078fce2eef03e2fab5b4#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in 8553bceb870c7954177eebe6528adf99ab47e84fd276078fce2eef03e2fab5b4#2 \
  --tx-in-script-file $beaconVaultScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $beaconRedeemer \
  --tx-in-collateral 62d4e442d8f01e035003fc60d448289440ca9b390c71385f11a55ac07b695ee0#2 \
  --mint "-1 ${beacon}" \
  --mint-script-file $beaconScriptFile \
  --mint-redeemer-file $beaconRedeemer \
  --change-address $(cat ../assets/wallets/02.addr) \
  --required-signer-hash $(cat ../assets/wallets/02.pkh) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"