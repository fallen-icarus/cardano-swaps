# Variables
dir="../assets/plutus-files/"
swapScriptFile="${dir}swap.plutus"
closeRedeemerFile="${dir}close.json"
beaconRedeemer="${dir}beaconRedeemer.json"
tmpDir="../assets/tmp/"

beaconScriptFile="${dir}beaconScript.plutus"
beaconSymbol="$(cat ${dir}beaconSymbol.txt)"
beaconVaultScriptFile="${dir}beaconVaultScript.plutus"
beaconVaultScriptAddrFile="${dir}beaconVaultScript.addr"

beacon="${beaconSymbol}.1111"

# Create Close redeemer file
cabal run -v0 cardano-swaps -- create-swap-redeemer \
  --close-swap \
  --out-file $closeRedeemerFile

# Create beacon redeemer
cabal run -v0 cardano-swaps -- create-beacon-redeemer \
  --burn-beacon 1111 \
  --out-file $beaconRedeemer

# Close the swap
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 30619c648d46a3e70682d28664aa9d63313a3cdfdc6d85b82ad3b7cab9cc9b63#3 \
  --tx-in 30619c648d46a3e70682d28664aa9d63313a3cdfdc6d85b82ad3b7cab9cc9b63#0 \
  --spending-tx-in-reference 30619c648d46a3e70682d28664aa9d63313a3cdfdc6d85b82ad3b7cab9cc9b63#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in 30619c648d46a3e70682d28664aa9d63313a3cdfdc6d85b82ad3b7cab9cc9b63#1 \
  --spending-tx-in-reference 30619c648d46a3e70682d28664aa9d63313a3cdfdc6d85b82ad3b7cab9cc9b63#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeRedeemerFile \
  --tx-in 30619c648d46a3e70682d28664aa9d63313a3cdfdc6d85b82ad3b7cab9cc9b63#2 \
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