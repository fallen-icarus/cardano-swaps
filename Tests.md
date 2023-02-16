# Tests

## Beacons
- [x] Unique beacon for every swap trading pair.

### Minting
- [x] Only one beacon can be minted per tx.
- [x] Only the beacon with the empty token name can be minted.
- [x] The beacon must be stored in an address locked by the Dapp's spending script for that swap pair.
  - [x] Fail if minted to an address locked by a pubkey.
  - [x] Fail if minted to an address locked by a non-dapp spending script.
  - [x] Fail if minted to a dapp address for a different swap pair.
- [x] The beacon must be stored with the reference script for the Dapp's spending script for that swap pair.
  - [x] Fail if not stored with a reference script.
  - [x] Fail if stored with a reference script for a non-dapp spending script.
  - [x] Fail if stored with a dapp reference script for a different swap pair.
- [x] The beacon must be minted to an address with a staking credential.
  - [x] Allow if stake pubkey used.
  - [x] Allow if staking script used.
  - [x] Fail if address doesn't have a staking credential
- [x] The beacon must be stored in a utxo containing the proper beacon symbol in the datum.
   - [x] Fail if different symbol in datum.
   - [x] Fail if swapBeacon == Nothing.
   - [x] Fail if beacon not stored with inline datum.
- [x] Fail if the burn redeemer is used to mint.

### Burning
- [x] Always allow burning.
  - [x] Allows burning many beacons.
  - [x] Allows burning a single beacon.
- [x] Must use burn redeemer.
  - [x] Fail if mint redeemer used to burn.

## DEX

### Close
- [x] All beacons in inputs must be burned.
  - [x] Allows closing non-beacon utxos.
- [x] Staking credential must approve.
  - [x] Stake pubkey must sign tx.
  - [x] Staking script must be executed in tx.
- [x] All outputs to address must contain proper datum.
  - [x] Fail if output datum is not inline.
  - [x] Fail if swapPrice <= 0.
  - [x] Fail if swapBeacon /= Nothing.

### Update
- [x] No beacons allowed in input.
- [x] Staking credential must approve.
  - [x] Stake pubkey must sign tx.
  - [x] Staking script must be executed in tx.
- [x] All outputs to address must contain proper datum.
  - [x] Fail if output datum is not inline.
  - [x] Fail if swapPrice <= 0.
  - [x] Fail if swapBeacon /= Nothing.

### Swap
- [x] No beacons allowed in input.
- [x] All swap input prices must be > 0.
- [x] All outputs to address must contain proper datum.
  - [x] Fail if output datum is not inline.
  - [x] Fail if swapPrice /= weighted avg.
  - [x] Fail if swapBeacon /= Nothing.
- [x] Only offered asset allowed to leave.
- [x] offered asset taken * price <= given asset