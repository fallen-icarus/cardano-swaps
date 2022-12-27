# Tests
## Swap Script Tests
### Basic Tests
#### Script Address
- [x] Every script configuration (owner,asked,offered) results in a unique swap script and swap script address

#### Info Redeemer
- [x] Correctly displays owner's pkh

#### Close Redeemer
- [x] Close redeemer fails if a non-owner tries using it
- [x] Close redeemer allows owner to withdraw all utxos (including the reference script utxo)

#### UpdatePrice Redeemer
- [x] Fails if an invalid price is given
- [x] Fails if the reference script is to be consumed
- [x] Fails if the outputs do not go to either the script or the owner
- [x] Fails if a non-owner tries using it
- [x] Fails if new datums don't match the price supplied to UpdatePrice redeemer
- [x] Fails if the new datum is not an inline-datum
- [x] Allows the owner to update all utxos (except the reference script utxo) at the script address with the new inline datum

#### Swap Redeemer
- [x] When ADA is asked for, the price (in ADA) is properly converted to lovelaces
- [x] When ADA is offered, the price (in ADA) is properly converted to lovelaces
- [x] When ADA is not part of the trading pair, the price is left as is
- [x] Fails if reference script utxo is to be consumed
- [x] Fails if change output to script contains a different datum than the input when consuming one utxo
- [x] Fails if change output to script does not contain the weighted price as datum when consuming multiple utxos
- [x] Fails if change output to script contains a non-inline-datum
- [x] Fails if a non-offered asset tries to be withdrawn
- [x] Fails if offered asset taken > asked asset given * price
- [x] Succeeds if no other errors and offered asset taken <= asked asset given * price
- [x] Succeeds when consuming multiple utxos with the same price for the datums
- [x] Succeeds when consuming multiple utxos with different prices for the datums

### Scenario Tests
#### Third party executes two complementary scripts in one tx; one utxo each
- [x] Third party pays fee
- [x] Third party can keep difference between asking prices (arbitrage)

#### Third party executes n complementary scripts in one tx; one utxo each
- [x] Third party pays fee
- [x] Third party can keep difference between asking prices (arbitrage)

#### Third party executes two complementary scripts in one tx; multiple utxos each
- [x] Third party pays fee
- [x] Third party can keep difference between asking prices (arbitrage)

#### Third part executes n complementary scripts in one tx; multiple utxos each
- [x] Third party pays fee
- [x] Third party can keep difference between asking prices (arbitrage)

## Staking Tests
- [x] Fails if non-owner tries delegating
- [ ] Fails if non-owner tries withdrawing rewards
- [x] Succeeds if owner tries delegating
- [ ] Succeeds if owner tries withdrawing rewards

## Beacon Tests
- [x] Cannot mint beacon token without depositing 1 ADA to beacon vault
- [x] Cannot burn beacon token without withdrawing 1 ADA from beacon vault
- [ ] Cannot spend reference script in beacon vault
- [x] Cannot deposit 1 ADA without correct datum
- [x] Cannot withdraw ADA from beacon vault without burning
- [x] Cannot mint more than one beacon at a time
- [x] Cannot burn more than one beacon at a time
- [x] Cannot mint any other tokens in the same transaction
- [x] Cannot burn any other tokens in the same transaction
- [x] Cannot use incorrect redeemers (will just get confusing error messages)
- [x] Cannot mint a beacon while using the wrong datum with the beacon vault

## Cli Tests
- [x] Catches invalid price
- [ ] Roundtrip template encoding/decoding for price calc

Current Tests:
1. swap(Tok2-ADA): owned by 01, testing staking based off just owner, tok2 = 4f74686572546f6b656e0a