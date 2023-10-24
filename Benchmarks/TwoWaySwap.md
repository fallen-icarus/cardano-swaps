# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

The universal swap spending script requires about 31 ADA to store on-chain.
The universal minting policy requires about 18 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### All swaps are for the same trading pair. The trading pair was (native asset,ADA).
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.289983 ADA | 0.434975 ADA |
| 10 | 0.810030 ADA | 1.215045 ADA |
| 20 | 1.387860 ADA | 2.081790 ADA |
| 30 | 1.966042 ADA | 2.949063 ADA |
| 32 | 2.023825 ADA | 3.035738 ADA |

The maximum number of swaps that could be created was 32.

#### All swaps are for different trading pairs.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.292235 ADA | 0.438353 ADA |
| 5 | 0.550943 ADA | 0.826415 ADA |
| 10 | 0.874526 ADA | 1.311789 ADA |
| 15 | 1.198240 ADA | 1.797360 ADA |
| 20 | 1.521955 ADA | 2.282933 ADA |
| 25 | 1.845714 ADA | 2.768571 ADA |

The maximum number of swaps that could be created was 25.



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
| 1 | 0.257676 ADA | 0.386514 ADA |
| 2 | 0.341731 ADA | 0.512597 ADA |
| 3 | 0.435310 ADA | 0.652965 ADA |
| 4 | 0.538324 ADA | 0.807486 ADA |
| 5 | 0.650819 ADA | 0.976229 ADA |
| 6 | 0.772793 ADA | 1.159190 ADA |
| 7 | 0.904248 ADA | 1.356372 ADA |
| 8 | 1.045182 ADA | 1.567773 ADA |
| 9 | 1.195596 ADA | 1.793394 ADA |
| 10 | 1.361519 ADA | 2.042279 ADA |
| 11 | 1.530893 ADA | 2.296340 ADA |

The maximum number of swaps that could fit in the transaction was 11.

#### BEST CASE SCENARIO: Execute multiple swap UTxOs for the different trading pairs.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.295228 ADA | 0.442842 ADA |
| 2 | 0.379152 ADA | 0.568728 ADA |
| 3 | 0.467304 ADA | 0.700956 ADA |
| 4 | 0.560613 ADA | 0.840920 ADA |
| 5 | 0.658194 ADA | 0.987291 ADA |
| 6 | 0.760049 ADA | 1.140074 ADA |
| 7 | 0.866176 ADA | 1.299264 ADA |
| 8 | 0.976576 ADA | 1.464864 ADA |
| 9 | 1.089480 ADA | 1.634220 ADA |
| 10 | 1.208514 ADA | 1.812771 ADA |
| 11 | 1.331820 ADA | 1.997730 ADA |
| 12 | 1.457631 ADA | 2.186447 ADA |
| 13 | 1.588599 ADA | 2.382899 ADA |
| 14 | 1.722955 ADA | 2.584433 ADA |

The maximum number of swaps that could fit in the transaction was 14.



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
| 52 | 1.560719 ADA | 2.341079 ADA | 

The maximum number of swaps that could be closed in the transaction was 52.

#### Closing swaps for different trading pairs.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198565 ADA | 0.297848 ADA |
| 10 | 0.358720 ADA | 0.538080 ADA |
| 20 | 0.581906 ADA | 0.872859 ADA |
| 30 | 0.853206 ADA | 1.279809 ADA |
| 40 | 1.172268 ADA | 1.758402 ADA |
| 50 | 1.539049 ADA | 2.308574 ADA | 
| 52 | 1.637040 ADA | 2.455560 ADA | 

The maximum number of swaps that could be closed in the transaction was 52.



## Updating swap prices
#### Updating swaps for the same trading pair.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.250678 ADA | 0.376017 ADA |
| 2 | 0.392486 ADA | 0.588729 ADA |
| 3 | 0.606631 ADA | 0.909947 ADA |
| 4 | 0.893116 ADA | 1.339674 ADA |
| 5 | 1.251939 ADA | 1.877909 ADA |

The maximum number of swaps that could be updated in the transaction was 5.

#### Updating swaps for different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.369978 ADA | 0.246652 ADA |
| 2 | 0.380428 ADA | 0.570642 ADA |
| 3 | 0.582538 ADA | 0.873807 ADA |
| 4 | 0.852983 ADA | 1.279475 ADA |
| 5 | 1.191760 ADA | 1.787640 ADA |

The maximum number of swaps that could be updated in the transaction was 5.



## Changing Swap Trading Pair
By composing both the `CreateSwap` minting redeemer and the `CloseOrUpdate` spending redeemer, it
is possible change what trading pair a swap is for in a single transaction (ie, you do not need to
first close the swap in one tx and then open the new swap in another tx).

Since there are many different scenarios that are possible, instead of testing all of them only the
worst possible scenario was benchmarked. All other scenarios should have better performance. If 
you are aware of a an even worse scenario, please open an issue so its benchmarks can be added.

#### All swaps start as different trading pairs and end as different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.294719 ADA | 0.442079 ADA |
| 2 | 0.468561 ADA | 0.702842 ADA |
| 3 | 0.710738 ADA | 1.066107 ADA |
| 4 | 1.021248 ADA | 1.531872 ADA |
| 5 | 1.400092 ADA | 2.100138 ADA |

The maximum number of swaps that could be updated in the transaction was 5.
