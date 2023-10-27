#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}oneWaySwap.addr"

swapRedeemerFile="${dir}closeOrUpdateOneWaySwap.json"

beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the CloseOrUpdate redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --close-or-update \
  --out-file $swapRedeemerFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"

# Creating the beacon policy redeemer. 
echo "Creating the beacon policy redeemer..."
cardano-swaps beacon-redeemers one-way \
  --burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli transaction build \
  --tx-in dfd8a4e30dd218da5909043e7e7389173269cf43def41c8994d2e0587531a764#2 \
  --tx-in dfd8a4e30dd218da5909043e7e7389173269cf43def41c8994d2e0587531a764#0 \
  --spending-tx-in-reference d52bf2621cd5ed984e529685e3a1088db81e4fa431769bec77f3374421b20fd7#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon}" \
  --mint-tx-in-reference d52bf2621cd5ed984e529685e3a1088db81e4fa431769bec77f3374421b20fd7#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address "$(cat ../../../ignored/wallets/01.addr)" \
  --required-signer-hash "$ownerPubKeyHash" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --signing-key-file ../../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
