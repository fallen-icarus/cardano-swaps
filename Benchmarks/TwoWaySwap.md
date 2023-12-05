# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

- The universal swap spending script requires about 24 ADA to store on-chain.
- The universal minting policy requires about 22 ADA to be stored on-chain.

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

#### Execute multiple swap UTxOs for the same trading pair and from the same address.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.239760 ADA | 0.359640 ADA |
| 2 | 0.297693 ADA | 0.446540 ADA |
| 3 | 0.356943 ADA | 0.535415 ADA |
| 4 | 0.417423 ADA | 0.626135 ADA |
| 5 | 0.479177 ADA | 0.718766 ADA |
| 6 | 0.542204 ADA | 0.813306 ADA |
| 7 | 0.606505 ADA | 0.909758 ADA |
| 8 | 0.672079 ADA | 1.008119 ADA |
| 9 | 0.738928 ADA | 1.108392 ADA |
| 10 | 0.813077 ADA | 1.219616 ADA |
| 15 | 1.172791 ADA | 1.759187 ADA |
| 20 | 1.570418 ADA | 2.355627 ADA |
| 25 | 1.994078 ADA | 2.991117 ADA |

The maximum number of swaps that could fit in the transaction was 25.

#### Execute multiple swap UTxOs for the different trading pairs.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.272797 ADA | 0.409196 ADA |
| 2 | 0.332174 ADA | 0.498261 ADA |
| 3 | 0.392825 ADA | 0.589238 ADA |
| 4 | 0.454749 ADA | 0.682124 ADA |
| 5 | 0.517948 ADA | 0.776922 ADA |
| 6 | 0.582419 ADA | 0.873629 ADA |
| 7 | 0.648165 ADA | 0.972248 ADA |
| 8 | 0.715184 ADA | 1.072776 ADA |
| 9 | 0.783476 ADA | 1.175214 ADA |
| 10 | 0.853131 ADA | 1.279697 ADA |
| 15 | 1.220507 ADA | 1.830761 ADA |
| 20 | 1.619725 ADA | 2.429588 ADA |
| 25 | 2.051048 ADA | 3.076572 ADA |

The maximum number of swaps that could fit in the transaction was 25.



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
| 55 | 1.604203 ADA | 2.406305 ADA | 

The maximum number of swaps that could be closed in the transaction was 55.

#### Closing swaps for different trading pairs.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198565 ADA | 0.297848 ADA |
| 10 | 0.358720 ADA | 0.538080 ADA |
| 20 | 0.581906 ADA | 0.872859 ADA |
| 30 | 0.853206 ADA | 1.279809 ADA |
| 40 | 1.172268 ADA | 1.758402 ADA |
| 50 | 1.539049 ADA | 2.308574 ADA | 
| 55 | 1.752569 ADA | 2.628854 ADA | 

The maximum number of swaps that could be closed in the transaction was 55.



## Updating swap prices
#### Updating swaps for the same trading pair.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.202881 ADA | 0.304322 ADA |
| 5 | 0.473208 ADA | 0.709812 ADA |
| 10 | 0.821743 ADA | 1.232615 ADA |
| 15 | 1.182208 ADA | 1.773312 ADA |
| 20 | 1.554603 ADA | 2.331905 ADA |
| 25 | 1.897395 ADA | 2.846093 ADA |
| 29 | 2.031634 ADA | 3.047451 ADA |

The maximum number of swaps that could be updated in the transaction was 29.

#### Updating swaps for different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.200459 ADA | 0.300689 ADA |
| 5 | 0.454681 ADA | 0.682022 ADA |
| 10 | 0.783085 ADA | 1.174628 ADA |
| 15 | 1.123418 ADA | 1.685127 ADA |
| 20 | 1.475681 ADA | 2.213522 ADA |
| 25 | 1.840094 ADA | 2.760141 ADA |
| 30 | 1.973048 ADA | 2.959572 ADA |

The maximum number of swaps that could be updated in the transaction was 30.



## Changing Swap Trading Pair
By composing both the `CreateOrCloseSwaps` minting redeemer and the `SpendWithMint` spending
redeemer, it is possible change what trading pair a swap is for in a single transaction (ie, you do
not need to first close the swap in one tx and then open the new swap in another tx).

Since there are many different scenarios that are possible, instead of testing all of them only the
worst possible scenario was benchmarked. All other scenarios should have better performance. If 
you are aware of an even worse scenario, please open an issue so its benchmarks can be added.

#### All swaps start as different trading pairs and end as different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.257566 ADA | 0.386349 ADA |
| 5 | 0.536339 ADA | 0.804509 ADA |
| 10 | 0.895587 ADA | 1.343381 ADA |
| 15 | 1.266721 ADA | 1.900082 ADA |
| 20 | 1.649784 ADA | 2.474676 ADA |
| 25 | 1.835465 ADA | 2.753198 ADA |
| 26 | 1.908041 ADA | 2.862062 ADA |

The maximum number of swaps that could be updated in the transaction was 26.
