# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

The universal swap spending script requires about 27 ADA to store on-chain.
The universal minting policy requires about 22 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### All swaps are for the same trading pair. The trading pair was (native asset,ADA).
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.274312 ADA | 0.411468 ADA |
| 10 | 0.616711 ADA | 0.925067 ADA |
| 20 | 0.997155 ADA | 1.495733 ADA |
| 30 | 1.377686 ADA | 1.066529 ADA |
| 40 | 1.764201 ADA | 2.646302 ADA |
| 47 | 2.030688 ADA | 3.046032 ADA |

The maximum number of swaps that could be created was 47.

#### All swaps are for different trading pairs.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.278405 ADA | 0.417608 ADA |
| 5 | 0.504180 ADA | 0.756270 ADA |
| 10 | 0.810578 ADA | 1.215867 ADA |
| 15 | 1.144332 ADA | 1.716498 ADA |
| 20 | 1.500373 ADA | 2.250560 ADA |
| 25 | 1.866583 ADA | 2.799875 ADA |
| 26 | 1.945556 ADA | 2.918334 ADA |

The maximum number of swaps that could be created was 26.



## Swap Assets
Swaps are validated by checking each output in the transaction. The checks are essentially:
1) Does this output have the beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?

Whenever a question is answered "No", the checks stop for that output and the validator moves on
to the next output. Because of this, the best possible performance is when all swaps are for unique
trading pairs (ie, the checks stop after the first question for all but the relevant output). The
worst possible performance is when all swaps are for the same trading pair AND are all from the
same address. All other scenarios will fall somewhere in between.

#### WORST CASE SCENARIO: Execute multiple swap UTxOs for the same trading pair and from the same address.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.248129 ADA | 0.372194 ADA |
| 2 | 0.322180 ADA | 0.483270 ADA |
| 3 | 0.405299 ADA | 0.607949 ADA |
| 4 | 0.497396 ADA | 0.746094 ADA |
| 5 | 0.598517 ADA | 0.897776 ADA |
| 6 | 0.708661 ADA | 1.062992 ADA |
| 7 | 0.827829 ADA | 1.241744 ADA |
| 8 | 0.956020 ADA | 1.424030 ADA |
| 9 | 1.093234 ADA | 1.639851 ADA |
| 10 | 1.250271 ADA | 1.875407 ADA |
| 11 | 1.406009 ADA | 2.109014 ADA |
| 12 | 1.570770 ADA | 2.356155 ADA |

The maximum number of swaps that could fit in the transaction was 12.

#### BEST CASE SCENARIO: Execute multiple swap UTxOs for the different trading pairs.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.284246 ADA | 0.426369 ADA |
| 2 | 0.357585 ADA | 0.536378 ADA |
| 3 | 0.434665 ADA | 0.651998 ADA |
| 4 | 0.515532 ADA | 0.773298 ADA |
| 5 | 0.600184 ADA | 0.900276 ADA |
| 6 | 0.688622 ADA | 1.032933 ADA |
| 7 | 0.780845 ADA | 1.171268 ADA |
| 8 | 0.876855 ADA | 1.315283 ADA |
| 9 | 0.976650 ADA | 1.464975 ADA |
| 10 | 1.080320 ADA | 1.620480 ADA |
| 11 | 1.187775 ADA | 1.781663 ADA |
| 12 | 1.299015 ADA | 1.948523 ADA |
| 13 | 1.414042 ADA | 2.121063 ADA |
| 14 | 1.542854 ADA | 2.299281 ADA |
| 15 | 1.655453 ADA | 2.483180 ADA |

The maximum number of swaps that could fit in the transaction was 15.



## Closing swaps
#### Closing swaps for the same trading pair.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.200986 ADA | 0.301479 ADA |
| 10 | 0.347039 ADA | 0.520559 ADA |
| 20 | 0.554603 ADA | 0.831905 ADA |
| 30 | 0.810590 ADA | 1.215885 ADA |
| 40 | 1.114472 ADA | 1.671708 ADA |
| 50 | 1.465852 ADA | 2.198778 ADA | 
| 53 | 1.580505 ADA | 2.370758 ADA | 

The maximum number of swaps that could be closed in the transaction was 53.

#### Closing swaps for different trading pairs.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198565 ADA | 0.297848 ADA |
| 10 | 0.358720 ADA | 0.538080 ADA |
| 20 | 0.581906 ADA | 0.872859 ADA |
| 30 | 0.853206 ADA | 1.279809 ADA |
| 40 | 1.172268 ADA | 1.758402 ADA |
| 50 | 1.539049 ADA | 2.308574 ADA | 
| 53 | 1.658389 ADA | 2.487584 ADA | 

The maximum number of swaps that could be closed in the transaction was 53.



## Updating swap prices
#### Updating swaps for the same trading pair.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.230873 ADA | 0.346310 ADA |
| 2 | 0.324874 ADA | 0.487311 ADA |
| 3 | 0.463214 ADA | 0.694821 ADA |
| 4 | 0.645892 ADA | 0.968838 ADA |
| 5 | 0.872909 ADA | 1.309364 ADA |
| 6 | 1.144263 ADA | 1.716395 ADA |
| 7 | 1.459956 ADA | 2.189934 ADA | 

The maximum number of swaps that could be updated in the transaction was 7.

#### Updating swaps for different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.225884 ADA | 0.338826 ADA |
| 5 | 0.467584 ADA | 0.701376 ADA |
| 10 | 0.911511 ADA | 1.367267 ADA |
| 15 | 1.513123 ADA | 2.269685 ADA |

The maximum number of swaps that could be updated in the transaction was 15.



## Changing Swap Trading Pair
By composing both the `CreateSwap` minting redeemer and the `CloseOrUpdate` spending redeemer, it
is possible change what trading pair a swap is for in a single transaction (ie, you do not need to
first close the swap in one tx and than open the new swap in another tx).

Since there are many different scenarios that are possible, instead of testing all of them only the
worst possible scenario was benchmarked. All other scenarios should have better performance. If 
you are aware of a an even worse scenario, please open an issue so its benchmarks can be added.

#### All swaps start as different trading pairs and end as different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.265904 ADA | 0.398856 ADA |
| 5 | 0.650808 ADA | 0.976212 ADA |
| 10 | 1.341457 ADA | 2.012186 ADA |
| 11 | 1.505360 ADA | 2.258040 ADA | 

The maximum number of swaps that could be updated in the transaction was 11.
