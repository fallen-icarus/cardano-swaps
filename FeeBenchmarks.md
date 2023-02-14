# Fee Benchmarks

- Maximum exUnitsMem' = 14000000
- Maximum exUnitsSteps' = 10000000000

---
## Creating a Live Address

*Initial Position* - a swappable utxo at a swap address created in the same tx as the beacon minting.

Each position is set to 10 ADA.

| Number of Initial Positions | Tx Fee |
|--|--|
| 0 | 0.598078 ADA |
| 1 | 0.608084 ADA |
| 2 | 0.618089 ADA |
| 3 | 0.628095 ADA |
| 4 | 0.638101 ADA |
| 5 | 0.648107 ADA |
| 6 | 0.658122 ADA |
| 7 | 0.668118 ADA |
| 8 | 0.678124 ADA |
| 9 | 0.688130 ADA |
| 10 | 0.698135 ADA |
| 11 | 0.708141 ADA |
| 12 | 0.718147 ADA |
| 13 | 0.728152 ADA |
| 14 | 0.738158 ADA |
| 15 | 0.748164 ADA |
| 16 | 0.758170 ADA |
| 17 | 0.768175 ADA |
| 18 | 0.778181 ADA |
| 19 | 0.788187 ADA |
| 20 | 0.798193 ADA |

---
## Updating Swaps

- Number of positions unchanged
- value of positions unchanged (10 ADA)
- user uses one of their own inputs and produces one of their own outputs

### Using a Reference Script

| Number of Positions Updated | Tx Fee | exUnitsMem' | exUnitsSteps' | Failure Reason |
|--|--|--|--|--|
| 1 | 0.260932 ADA | unknown | unknown | n/a |
| 2 | 0.419719 ADA | unknown | unknown | n/a |
| 3 | 0.665222 ADA | unknown | unknown | n/a |
| 4 | 1.004827 ADA | unknown | unknown | n/a |
| 5 | ExUnitsTooBigUTxO | 15628835 | 4605866645 | exUnitsMem' |
| 6 | ExUnitsTooBigUTxO | 22546986 | 6612463494 | exUnitsMem' |
| 7 | ExUnitsTooBigUTxO | 30953321 | 9040607107 | exUnitsMem' |
| 8 | ExUnitsTooBigUTxO | 40943864 | 11915899880 | Both |
| 9 | ExUnitsTooBigUTxO | 52614639 | 15263944209 | Both |
| 10 | ExUnitsTooBigUTxO | 66061670 | 19110342490 | Both |

### Using a Local Script

| Number of Positions Updated | Tx Fee | exUnitsMem' | exUnitsSteps' | Failure Reason |
|--|--|--|--|--|
| 1 | 0.466035 ADA | unknown | unknown | n/a |
| 2 | 0.616697 ADA | unknown | unknown | n/a |
| 3 | 0.853542 ADA | unknown | unknown | n/a |
| 4 | 1.183957 ADA | unknown | unknown | n/a |
| 5 | ExUnitsTooBigUTxO | 15102135 | 4426997895 | exUnitsMem' |
| 6 | ExUnitsTooBigUTxO | 21895134 | 6391530060 | exUnitsMem' |
| 7 | ExUnitsTooBigUTxO | 30169713 | 8775512011 | exUnitsMem' |
| 8 | ExUnitsTooBigUTxO | 40021896 | 11604546144 | Both |
| 9 | ExUnitsTooBigUTxO | 51547707 | 14904234855 | Both |
| 10 | ExUnitsTooBigUTxO | 64843170 | 18700180540 | Both |

---
## Swap

- Swap with a single address (implies single trading pair)
- all swap inputs consolidated into one swap output (nothing actually swapped)
- all swap inputs have the same asking price and same value (price = 2, value = 10 ADA)
- user uses 2 of their own inputs and produces 2 of their own outputs

*Swap Input* - a utxo from a swap address used as input to a tx.

### Using a Reference Script

| Number of Swap Inputs | Tx Fee | exUnitsMem' | exUnitsSteps' | Failure Reason |
|--|--|--|--|--|
| 1 | 0.354529 ADA | unknown | unknown | n/a |
| 2 | 0.611316 ADA | unknown | unknown | n/a |
| 3 | 0.970329 ADA | unknown | unknown | n/a |
| 4 | ExUnitsTooBigUTxO | 15689680 | 4616591484 | exUnitsMem' |
| 5 | ExUnitsTooBigUTxO | 23069860 | 6804256625 | exUnitsMem' |
| 6 | ExUnitsTooBigUTxO | 31989156 | 9439338078 | exUnitsMem' |
| 7 | ExUnitsTooBigUTxO | 42609658 | 12578963117 | Both |
| 8 | ExUnitsTooBigUTxO | 54949312 | 16211477464 | Both |
| 9 | ExUnitsTooBigUTxO | 69194232 | 20405471985 | Both |
| 10 | ExUnitsTooBigUTxO | 85338340 | 25137828810 | Both |

### Using a Local Script

| Number of Swap Inputs | Tx Fee | exUnitsMem' | exUnitsSteps' | Failure Reason |
|--|--|--|--|--|
| 1 | 0.560431 ADA | unknown | unknown | n/a |
| 2 | 0.810423 ADA | unknown | unknown | n/a |
| 3 | 1.162641 ADA | unknown | unknown | n/a |
| 4 | ExUnitsTooBigUTxO | 15182468 | 4446235770 | exUnitsMem' |
| 5 | ExUnitsTooBigUTxO | 22262490 | 6536266310 | exUnitsMem' |
| 6 | ExUnitsTooBigUTxO | 30842004 | 9061131294 | exUnitsMem' |
| 7 | ExUnitsTooBigUTxO | 41083100 | 12077957996 | Both |
| 8 | ExUnitsTooBigUTxO | 53003724 | 15575092138 | Both |
| 9 | ExUnitsTooBigUTxO | 66789990 | 19621124586 | Both |
| 10 | ExUnitsTooBigUTxO | 82435820 | 24192937470 | Both |

---
## Close

- all inputs consolidated into one output at user's pubkey address
- all swap positions have the same value and price (price = 2, value = 10 ADA)
- the beacon burned (and reference script removed) in the same tx

### Using Reference Script

| Number of Swaps Closed | Tx Fee | exUnitsMem' | exUnitsSteps' | Failure Reason |
|--|--|--|--|--|
| 1 | 0.588052 ADA | unknown | unknown | n/a |
| 2 | 0.702725 ADA | unknown | unknown | n/a |
| 3 | 0.836737 ADA | unknown | unknown | n/a |
| 4 | 0.990089 ADA | unknown | unknown | n/a |
| 5 | 1.162779 ADA | unknown | unknown | n/a |
| 6 | 1.354809 ADA | unknown | unknown | n/a |
| 7 | ExUnitsTooBigUTxO | 15038344 | 4782533081 | ExUnitsMem' |
| 8 | ExUnitsTooBigUTxO | 17848737 | 5700935359 | ExUnitsMem' |
| 9 | ExUnitsTooBigUTxO | 20895278 | 6698582139 | ExUnitsMem' |
| 10 | ExUnitsTooBigUTxO | 24177967 | 7775473421 | ExUnitsMem' |

### Using a Local Script

This test cannot be done because consuming the reference script utxo seems to require actually using the reference script. If you try to use a local file to spend this utxo, you will get an ExtraneousScriptWitnessesUTXOW error.