# Benchmarks (YMMV)

## Opening Swap Addresses

### Setup
1. A beacon was minted for `TestToken1 -> Lovelace` and stored at the proper address with the reference script.
2. All positions created have the price of `1 / 1000000` and have the value of 10 ADA.
3. The node emulator was used with the same configurations as the mainnet.
4. Minting the beacon was done using a local minting script. Performance would be better if a reference script was used.

### Results
| Number of positions created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.518897 ADA | 0.778346 ADA |
| 5 | 0.542464 ADA | 0.813696 ADA |
| 10 | 0.577994 ADA | 0.866991 ADA |
| 15 | 0.607453 ADA | 0.911180 ADA |
| 20 | 0.642983 ADA | 0.964475 ADA |
| 25 | 0.672486 ADA | 1.008729 ADA |

Tests were stopped after opening 25 positions since that is already over-kill for how the DEX is intended to be used. The transaction still has most of its execution units available for more positions.

## Closing Swap Addresses

### Setup
1. A mock address was created without a reference script stored in the beacon UTxO.
2. The swap script and beacon were for `TestToken1 -> Lovelace`.
3. Each position to be closed had a price of `1 / 1000000` and a value of 10 ADA.
4. Local scripts were used to burn the beacon and close the positions.
5. Closing entails burning the beacon and the specified number of open positions. No UTxOs were locked at the swap address by the transaction.

### Results
| Number of Positions Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.528589 ADA | 0.792884 ADA |
| 5 | 0.600765 ADA | 0.901148 ADA |
| 10 | 0.705872 ADA | 1.058808 ADA |
| 15 | 0.827519 ADA | 1.241279 ADA |
| 20 | 0.965706 ADA | 1.448559 ADA |

Closing 21 open positions in addition to burning the beacon exceeded the transaction limits. You can get better performance by using reference scripts.

## Updating Positions

### Setup
1. A mock address was created without a reference script stored in the beacon UTxO.
2. The swap script and beacon were for `TestToken1 -> Lovelace`.
3. Each position to be updated had a price of `1 / 1000000` and a value of 10 ADA.
4. Each position was updated to have a price of `2 / 1000000` and a value of 10 ADA.
5. A local script was used to update the positions.
6. Every swap input to be updated had one corresponding swap output.

### Results
| Number of Positions Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.413065 ADA | 0.619598 ADA |
| 5 | 0.629606 ADA | 0.944409 ADA |
| 10 | 1.041516 ADA | 1.562274 ADA |
| 15 | 1.648797 ADA | 2.473196 ADA |

Updating 16 positions caused the transaction to exceed its limits. The above should be considered worst case scenario. Better performance can be achieved by consolidating positions and/or using reference scripts.

## Composing Swaps
These benchmarks show how many currency conversions can be composed in a single transaction. Currency conversions are direction specific: ADA -> DJED is a different currency conversion than DJED -> ADA.

### Setup
1. All conversions had one input and one corresponding output. So a transaction with 2 conversions had two swap inputs and two swap outputs. 
2. Reference scripts were used for all swap scripts.
3. The node emulator was used with the same configurations as the mainnet.

| Individual Conversions | Price (Ask/Offer) |
|:--:|:--:|
| TestToken1 -> Lovelace | 1 / 1000000 |
| TestToken2 -> TestToken1 | 1 / 1 |
| TestToken3 -> TestToken2 | 1 / 1 |
| TestToken4 -> TestToken3 | 1 / 1 |
| TestToken5 -> TestToken4 | 1 / 1 |
| TestToken6 -> TestToken5 | 1 / 1 |
| TestToken7 -> TestToken6 | 1 / 1 |
| TestToken8 -> TestToken7 | 1 / 1 |
| TestToken9 -> TestToken8 | 1 / 1 |
| TestToken10 -> TestToken9 | 1 / 1 |
| TestToken11 -> TestToken10 | 1 / 1 |
| TestToken12 -> TestToken11 | 1 / 1 |
| TestToken13 -> TestToken12 | 1 / 1 |
| TestToken14 -> TestToken13 | 1 / 1 |

The prices should be read as "Alice deposits 1 TestToken1 to the swap address and receives 1000000 lovelace."

### Results
| Number of Conversions | Tx Fee | Collateral Required | Conversion |
|:--:|:--:|:--:|:--:|
| 1 | 0.248934 ADA | 0.373401 ADA | TestToken1 -> Lovelace |
| 2 | 0.313976 ADA | 0.470964 ADA | TestToken2 -> Lovelace |
| 3 | 0.383768 ADA | 0.575652 ADA | TestToken3 -> Lovelace |
| 4 | 0.458311 ADA | 0.687467 ADA | TestToken4 -> Lovelace |
| 5 | 0.537604 ADA | 0.806406 ADA | TestToken5 -> Lovelace |
| 6 | 0.621646 ADA | 0.932469 ADA | TestToken6 -> Lovelace |
| 7 | 0.709116 ADA | 1.063674 ADA | TestToken7 -> Lovelace |
| 8 | 0.802660 ADA | 1.203990 ADA | TestToken8 -> Lovelace |
| 9 | 0.900954 ADA | 1.351431 ADA | TestToken9 -> Lovelace |
| 10 | 1.004577 ADA | 1.506866 ADA | TestToken10 -> Lovelace |
| 11 | 1.112415 ADA | 1.668623 ADA | TestToken11 -> Lovelace |
| 12 | 1.225004 ADA | 1.837506 ADA | TestToken12 -> Lovelace |
| 13 | 1.342342 ADA | 2.013513 ADA | TestToken13 -> Lovelace |
| 14 | 1.464431 ADA | 2.196647 ADA | TestToken14 -> Lovelace |

The execution limits were exceeded with 15 conversions in a single transaction.

## Aggregating Swaps
If Alice has 10,000 ADA to convert to DJED but each available swap is only for 1,000 DJED, Alice will need to swap with multiple UTxOs for the same currency conversion. This will be referred to as aggregating swaps.

### Setup
1. All swaps had one input and one corresponding output. So a transaction with 2 conversions had two swap inputs and two swap outputs. 
2. Reference scripts were used for all swap scripts.
3. The node emulator was used with the same configurations as the mainnet.
4. All swap UTxOs were for `TestToken1 -> Lovelace` at a price of `1 / 1000000` and located at the same address. The value for each UTxO position was 10 ADA.

### Results
| Number of Swaps Aggregated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.248934 ADA | 0.373401 ADA |
| 2 | 0.375300 ADA | 0.562950 ADA |
| 3 | 0.577799 ADA | 0.866699 ADA |
| 4 | 0.856654 ADA | 1.284981 ADA |
| 5 | 1.208510 ADA | 1.812765 ADA |

Aggregating 6 swaps exceeded the transactions execution limits.

### Comments
When there are multiple input UTxOs from a given swap address, the script must calculate the weighted avg price for the inputs. This calculation is fairly intensive and is why the transaction exceeds its limits faster in the above benchmark tests.

The prices themselves are largely irrelevant for this calculation. Instead, the biggest cost savings is to take UTxOs for the same currency conversion **but from different addresses**. The reason for this is that the script can avoid doing the weighted avg price calculation if there is only one input from a given address. To demonstrate this, the `Aggregating Swaps` benchmarks were redone.

The first test is exactly the same as the above except for setup number 4. Instead, all swap UTxOs were for `TestToken2 -> TestToken1` at a price of `1 / 1` and still located at the same address. This test is meant to show that the prices do not impact the performance of the DEX.

The second test is exactly the same as the original except the swap UTxOs for `TestToken1 -> Lovelaces` were distributed among different user addresses. The prices and UTxO values were the same as the original test.

#### Results for simplified price
| Number of Swaps Aggregated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.252325 ADA | 0.378488 ADA |
| 2 | 0.394946 ADA | 0.592419 ADA |
| 3 | 0.622916 ADA | 0.934374 ADA |
| 4 | 0.935757 ADA | 1.403636 ADA |
| 5 | 1.333469 ADA | 2.000204 ADA |

Tx limits exceeded for 6 swaps aggregated.

#### Results for UTxOs from different addresses
| Number of Swaps Aggregated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.248934 ADA | 0.373401 ADA |
| 2 | 0.309001 ADA | 0.463502 ADA |
| 3 | 0.373818 ADA | 0.560727 ADA |
| 4 | 0.443385 ADA | 0.665078 ADA |
| 5 | 0.519025 ADA | 0.778538 ADA |
| 6 | 0.598093 ADA | 0.897140 ADA |
| 7 | 0.681911 ADA | 1.022867 ADA |
| 8 | 0.771197 ADA | 1.156796 ADA |
| 9 | 0.863797 ADA | 1.295696 ADA |

The benchmarks stop after 9 addresses because the emulator ran out of usable addresses. However, the tx's memory usage was only at 50% capacity. Based on the observation that these fees are very similar to the composable swap fees, a fair estimate would be that about 14 UTxOs is the max.

To summarize these results, this DEX favors satisfying many users, each with only a single position, over satisfying one user with many open positions.