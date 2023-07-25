#!/bin/sh

# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

swapAddr1="addr_test1zqvkss4kwg5y0h5qxxw7fe7rf02fmcxs0x2knew3yzvzad3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqykhv32"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}swap.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps swap-redeemer \
  --swap \
  --out-file $swapRedeemerFile

# Create the new swap datum. The CLI can calculate the weighted avg price for you.
echo "Creating the new swap datum..."
cardano-swaps swap-datum \
  --offer-lovelace \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --utxo-balance 10000000 \
  --price-numerator 1 \
  --price-denominator 50000 \
  --utxo-balance 10000000 \
  --price-numerator 1 \
  --price-denominator 100000 \
  --out-file $swapDatumFile1

# Helper beacon variables.
beaconPolicyId1=$(cardano-swaps beacon-info policy-id \
  --offer-lovelace \
  --stdout)

beaconName1=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --stdout)

beacon1="${beaconPolicyId1}.${beaconName1}"

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 8c3536aecc6cf25c2e4bdef905182fddca6b8f289ceea2c42ccbed46877ca470#1 \
  --tx-in 41b4fb1b684bd47571ebfd41a53e26b1968d34e38ab92dbb4f0d0e1d29e1bb06#2 \
  --tx-in 8c3536aecc6cf25c2e4bdef905182fddca6b8f289ceea2c42ccbed46877ca470#0 \
  --spending-tx-in-reference c774e01a1f0e4cd06d62780dcd52f6d00290b8217ac0e538747bf79d1a49dbfb#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-in 31c5603bb70974aad4ef47e48e6f83ff6e931bcdbc6093b40500524db941bf67#0 \
  --spending-tx-in-reference c774e01a1f0e4cd06d62780dcd52f6d00290b8217ac0e538747bf79d1a49dbfb#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$swapAddr1 + 10000000 lovelace + 2 ${beacon1} + 150 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 3000000 + 300 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address "$(cat ../assets/wallets/01.addr)" \
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