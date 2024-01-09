#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"

beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --close \
  --out-file $swapRedeemerFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"

# Creating the beacon script redeemer. 
echo "Creating the beacon script redeemer..."
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli transaction build \
  --tx-in 44101845b0301455ec2a3dd7b98a3b22623011fb38a6216ae1fa78358c5a61fc#1 \
  --tx-in 4cbb75a3ee32c6d6b0a79bb3fff90c009e13f20772c21a8c668c8e94e02d0212#0 \
  --spending-tx-in-reference 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon} + -1 ${askBeacon}" \
  --mint-tx-in-reference 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
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
