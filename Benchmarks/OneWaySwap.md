# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

The universal swap spending script requires about 31 ADA to store on-chain.
The universal minting policy requires about 16 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### Here, all swaps are for the same trading pair (native asset,ADA).
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.282218 ADA | 0.423327 ADA |
| 10 | 0.732779 ADA | 1.099169 ADA |
| 20 | 1.233402 ADA | 1.850103 ADA |
| 30 | 1.734201 ADA | 2.601302 ADA |
| 33 | 1.890636 ADA | 2.835954 ADA |

The maximum number of swaps that could be created was 33.

#### Here, all swaps are for different trading pairs.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.283829 ADA | 0.425744 ADA |
| 10 | 0.790601 ADA | 1.185902 ADA |
| 20 | 1.254414 ADA | 2.031621 ADA |
| 25 | 1.636365 ADA | 2.454548 ADA |

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
| 1 | 0.248925 ADA | 0.373388 ADA |
| 2 | 0.323246 ADA | 0.484869 ADA |
| 3 | 0.406107 ADA | 0.609161 ADA |
| 4 | 0.497421 ADA | 0.746132 ADA |
| 5 | 0.597231 ADA | 0.895847 ADA |
| 6 | 0.705538 ADA | 1.058307 ADA |
| 7 | 0.822342 ADA | 1.233513 ADA |
| 8 | 0.947643 ADA | 1.421465 ADA |
| 9 | 1.081440 ADA | 1.622160 ADA |
| 10 | 1.234534 ADA | 2.851801 ADA |
| 11 | 1.385802 ADA | 2.078703 ADA |
| 12 | 1.545567 ADA | 2.318251 ADA |

The maximum number of swaps that could fit in the transaction was 12.

#### BEST CASE SCENARIO: Execute multiple swap UTxOs for the different trading pairs.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.285138 ADA | 0.427707 ADA |
| 2 | 0.359413 ADA | 0.539120 ADA |
| 3 | 0.437965 ADA | 0.656948 ADA |
| 4 | 0.519464 ADA | 0.779196 ADA |
| 5 | 0.605678 ADA | 0.908517 ADA |
| 6 | 0.694839 ADA | 1.042259 ADA |
| 7 | 0.788714 ADA | 1.183071 ADA |
| 8 | 0.885421 ADA | 1.329632 ADA |
| 9 | 0.987958 ADA | 1.481937 ADA |
| 10 | 1.092923 ADA | 1.639385 ADA |
| 11 | 1.201718 ADA | 1.802577 ADA |
| 12 | 1.313461 ADA | 1.970192 ADA |
| 13 | 1.429034 ADA | 2.143551 ADA |
| 14 | 1.548438 ADA | 2.322657 ADA |

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
| 54 | 1.560719 ADA | 2.341079 ADA | 

The maximum number of swaps that could be closed in the transaction was 54.

#### Closing swaps for different trading pairs.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198565 ADA | 0.297848 ADA |
| 10 | 0.358720 ADA | 0.538080 ADA |
| 20 | 0.581906 ADA | 0.872859 ADA |
| 30 | 0.853206 ADA | 1.279809 ADA |
| 40 | 1.172268 ADA | 1.758402 ADA |
| 50 | 1.539049 ADA | 2.308574 ADA | 
| 53 | 1.729972 ADA | 2.594958 ADA | 

The maximum number of swaps that could be closed in the transaction was 53.



## Updating swap prices
#### Updating swaps for the same trading pair.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.234770 ADA | 0.352155 ADA |
| 2 | 0.337576 ADA | 0.506364 ADA |
| 3 | 0.489626 ADA | 0.734439 ADA |
| 4 | 0.690921 ADA | 1.036382 ADA |
| 5 | 0.941461 ADA | 1.412192 ADA |
| 6 | 1.241245 ADA | 1.861868 ADA |

The maximum number of swaps that could be updated in the transaction was 6.

#### Updating swaps for different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.231086 ADA | 0.346629 ADA |
| 2 | 0.326535 ADA | 0.489803 ADA |
| 3 | 0.467558 ADA | 0.701337 ADA |
| 4 | 0.654152 ADA | 0.981228 ADA |
| 5 | 0.886320 ADA | 1.329480 ADA |
| 6 | 1.164061 ADA | 1.746092 ADA |

The maximum number of swaps that could be updated in the transaction was 6.



## Changing Swap Trading Pair
By composing both the `CreateSwap` minting redeemer and the `CloseOrUpdate` spending redeemer, it is possible to change a swap's trading pair in a single transaction (ie, you do not need to first close the swap in one tx and then open the new swap in another tx).

Since there are too many scenarios to test all of them, only the
worst possible scenarios for each action were benchmarked. All other scenarios should have better performance. If you are aware of an even worse scenario, please open an issue so its benchmarks can be added.

#### All swaps start as different trading pairs and end as different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.264537 ADA | 0.396806 ADA |
| 2 | 0.385547 ADA | 0.578321 ADA |
| 3 | 0.552130 ADA | 0.828195 ADA |
| 4 | 0.764286 ADA | 1.146429 ADA |
| 5 | 1.022015 ADA | 1.533023 ADA |

The maximum number of swaps that could be updated in the transaction was 5.
