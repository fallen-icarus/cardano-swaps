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
| 1 | 0.249986 ADA | 0.374979 ADA |
| 2 | 0.320817 ADA | 0.481226 ADA |
| 3 | 0.395639 ADA | 0.593459 ADA |
| 4 | 0.474365 ADA | 0.711548 ADA |
| 5 | 0.557037 ADA | 0.835556 ADA |
| 6 | 0.643657 ADA | 0.965486 ADA |
| 7 | 0.734223 ADA | 1.101335 ADA |
| 8 | 0.828737 ADA | 1.243106 ADA |
| 9 | 1.927197 ADA | 1.390796 ADA |
| 10 | 1.035633 ADA | 1.553450 ADA |
| 11 | 1.141988 ADA | 1.712982 ADA |
| 12 | 1.252290 ADA | 1.878435 ADA |
| 13 | 1.366539 ADA | 2.049809 ADA |
| 14 | 1.484735 ADA | 2.227103 ADA |
| 15 | 1.606878 ADA | 2.410317 ADA |
| 16 | 1.732968 ADA | 2.599452 ADA |
| 17 | 1.792753 ADA | 2.689130 ADA |

The maximum number of swaps that could fit in the transaction was 17.

#### Execute multiple swap UTxOs for the different trading pairs.
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
| 15 | 1.713331 ADA | 2.569997 ADA |

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
| 1 | 0.202054 ADA | 0.303081 ADA |
| 5 | 0.454814 ADA | 0.682221 ADA |
| 10 | 0.781391 ADA | 1.172087 ADA |
| 15 | 1.119898 ADA | 1.679847 ADA |
| 20 | 1.470334 ADA | 2.205501 ADA |
| 25 | 1.832920 ADA | 2.749380 ADA |
| 26 | 1.906869 ADA | 2.860304 ADA |

The maximum number of swaps that could be updated in the transaction was 26.

#### Updating swaps for different trading pairs.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199632 ADA | 0.299448 ADA |
| 5 | 0.436989 ADA | 0.655484 ADA |
| 10 | 0.744312 ADA | 1.116468 ADA |
| 15 | 1.063565 ADA | 1.595348 ADA |
| 20 | 1.394747 ADA | 2.092121 ADA |
| 25 | 1.738079 ADA | 2.607119 ADA |
| 28 | 1.878664 ADA | 2.817996 ADA |

The maximum number of swaps that could be updated in the transaction was 28.



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
