#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

beaconScriptFile="${dir}oneWayBeacons.plutus"
beaconAddrFile="${dir}oneWayBeaconStake.addr"

swapAddrFile="${dir}oneWaySwap.addr"

swapDatumFile="${dir}swapDatum.json"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"
beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconScriptFile

# Create the meta beacon stake address.
echo "Creating the beacon reward address..."
cardano-cli stake-address build \
  --stake-script-file $beaconScriptFile \
  --testnet-magic 1 \
  --out-file $beaconAddrFile

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --update-with-stake \
  --out-file $swapRedeemerFile

# Create the beacon script redeemer.
echo "Creating the beacon redeemer..."
cardano-swaps beacon-redeemers one-way \
  --update-only \
  --out-file $beaconRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --price-numerator 2000000 \
  --price-denominator 1 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-lovelace \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"

# Create the transaction.
cardano-cli transaction build \
  --tx-in a475dde6c24ad47857ba7c9ddc98fd86cb5adedaa0a8e0ab797a21aa72ad3f2c#1 \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#0 \
  --spending-tx-in-reference 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#1 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
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
