# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

- The universal swap spending script requires about 22 ADA to store on-chain.
- The universal minting policy requires about 20 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### All swaps are for the same trading pair. The trading pair was (native asset,ADA).
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.282218 ADA | 0.423327 ADA |
| 10 | 0.732779 ADA | 1.099169 ADA |
| 20 | 1.233402 ADA | 1.850103 ADA |
| 30 | 1.734201 ADA | 2.601302 ADA |
| 33 | 1.890636 ADA | 2.835954 ADA |

The maximum number of swaps that could be created was 33.

#### All swaps are for different trading pairs.
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

#### Execute multiple swap UTxOs for the same trading pair and from the same address.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.237237 ADA | 0.355856 ADA |
| 2 | 0.292634 ADA | 0.438951 ADA |
| 3 | 0.349335 ADA | 0.524003 ADA |
| 4 | 0.407354 ADA | 0.610881 ADA |
| 5 | 0.466433 ADA | 0.699650 ADA |
| 6 | 0.526874 ADA | 0.790311 ADA |
| 7 | 0.588575 ADA | 0.882863 ADA |
| 8 | 0.651537 ADA | 0.977306 ADA |
| 9 | 0.715761 ADA | 1.073642 ADA |
| 10 | 0.787273 ADA | 1.180910 ADA |
| 15 | 1.133608 ADA | 1.700412 ADA |
| 20 | 1.517539 ADA | 2.276309 ADA |
| 25 | 1.927185 ADA | 2.890778 ADA |
| 26 | 2.012932 ADA | 3.019398 ADA |

The maximum number of swaps that could fit in the transaction was 26.

#### Execute multiple swap UTxOs for the different trading pairs.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.237237 ADA | 0.355856 ADA |
| 2 | 0.292634 ADA | 0.438951 ADA |
| 3 | 0.349335 ADA | 0.524003 ADA |
| 4 | 0.407354 ADA | 0.610881 ADA |
| 5 | 0.466433 ADA | 0.699650 ADA |
| 6 | 0.526874 ADA | 0.790311 ADA |
| 7 | 0.588575 ADA | 0.882863 ADA |
| 8 | 0.651537 ADA | 0.977306 ADA |
| 9 | 0.715761 ADA | 1.073642 ADA |
| 10 | 0.787273 ADA | 1.180910 ADA |
| 15 | 1.133608 ADA | 1.700412 ADA |
| 20 | 1.517539 ADA | 2.276309 ADA |
| 25 | 1.927185 ADA | 2.890778 ADA |
| 26 | 2.012932 ADA | 3.019398 ADA |

The maximum number of swaps that could fit in the transaction was 26.



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
| 56 | 1.610740 ADA | 2.416110 ADA | 

The maximum number of swaps that could be closed in the transaction was 56.

#### Closing swaps for different trading pairs.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198565 ADA | 0.297848 ADA |
| 10 | 0.358720 ADA | 0.538080 ADA |
| 20 | 0.581906 ADA | 0.872859 ADA |
| 30 | 0.853206 ADA | 1.279809 ADA |
| 40 | 1.172268 ADA | 1.758402 ADA |
| 50 | 1.539049 ADA | 2.308574 ADA | 
| 56 | 1.759106 ADA | 2.638659 ADA | 

The maximum number of swaps that could be closed in the transaction was 56.



## Updating swap prices
#### Updating swaps for the same trading pair.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.201809 ADA | 0.302714 ADA |
| 5 | 0.425520 ADA | 0.638280 ADA |
| 10 | 0.715786 ADA | 1.073679 ADA |
| 15 | 1.017981 ADA | 1.526972 ADA |
| 20 | 1.332106 ADA | 1.998159 ADA |
| 25 | 1.658381 ADA | 2.487572 ADA |
| 30 | 2.002657 ADA | 3.003986 ADA |
| 31 | 2.071729 ADA | 3.107594 ADA |

The maximum number of swaps that could be updated in the transaction was 31.

#### Updating swaps for different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199632 ADA | 0.299448 ADA |
| 5 | 0.406626 ADA | 0.609939 ADA |
| 10 | 0.676344 ADA | 1.014516 ADA |
| 15 | 0.958169 ADA | 1.437254 ADA |
| 20 | 1.251923 ADA | 1.877885 ADA |
| 25 | 1.557827 ADA | 2.336741 ADA |
| 30 | 1.875660 ADA | 2.813490 ADA |
| 32 | 2.006134 ADA | 3.009201 ADA |

The maximum number of swaps that could be updated in the transaction was 32.



## Changing Swap Trading Pair
By composing both the `CreateOrCloseSwaps` minting redeemer and the `SpendWithMint` spending
redeemer, it is possible change what trading pair a swap is for in a single transaction (ie, you do
not need to first close the swap in one tx and then open the new swap in another tx).

Since there are many different scenarios that are possible, instead of testing all of them only the
worst possible scenario was benchmarked. All other scenarios should have better performance. If you
are aware of an even worse scenario, please open an issue so its benchmarks can be added.

#### All swaps start as different trading pairs and end as different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.253238 ADA | 0.379857 ADA |
| 5 | 0.515147 ADA | 0.772721 ADA |
| 10 | 0.853314 ADA | 1.279971 ADA |
| 15 | 1.203367 ADA | 1.805051 ADA |
| 20 | 1.565349 ADA | 2.348024 ADA |
| 25 | 1.939481 ADA | 2.909222 ADA |
| 26 | 2.015739 ADA | 3.023609 ADA |

The maximum number of swaps that could be updated in the transaction was 26.
