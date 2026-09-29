# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

- The universal swap spending script requires 17.783060 ADA to store on-chain.
- The universal minting policy requires 19.670840 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### All swaps are for the same trading pair. The trading pair was (native asset,ADA).

| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.235520 ADA | 0.353280 ADA |
| 10 | 0.619708 ADA | 0.929562 ADA |
| 20 | 1.046583 ADA | 1.569875 ADA |
| 30 | 1.473634 ADA | 2.210451 ADA |
| 34 | 1.644384 ADA | 2.466576 ADA |

The maximum number of swaps that could be created was 34.

#### All swaps are for different trading pairs.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.289477 ADA | 0.434216 ADA |
| 5 | 0.486533 ADA | 0.729800 ADA |
| 10 | 0.732661 ADA | 1.098992 ADA |
| 15 | 0.979362 ADA | 1.469043 ADA |
| 20 | 1.225850 ADA | 1.838775 ADA |
| 24 | 1.422854 ADA | 2.134281 ADA |

The maximum number of swaps that could be created was 24.

## Swap Assets

Swaps are validated by checking each output in the transaction. The checks are essentially:

1) Does this output have the beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?

#### Execute multiple swap UTxOs for the same trading pair and from the same address.

| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.230416 ADA | 0.345624 ADA |
| 2 | 0.282736 ADA | 0.424104 ADA |
| 3 | 0.336453 ADA | 0.504680 ADA |
| 4 | 0.391567 ADA | 0.587351 ADA |
| 5 | 0.448122 ADA | 0.672183 ADA |
| 6 | 0.506031 ADA | 0.759047 ADA |
| 7 | 0.565337 ADA | 0.848006 ADA |
| 8 | 0.626041 ADA | 0.939062 ADA |
| 9 | 0.688142 ADA | 1.032213 ADA |
| 10 | 0.751640 ADA | 1.127460 ADA |
| 15 | 1.090089 ADA | 1.635134 ADA |
| 20 | 1.463471 ADA | 2.195207 ADA |
| 25 | 1.872050 ADA | 2.808075 ADA |
| 28 | 2.133938 ADA | 3.200907 ADA |

The maximum number of swaps that could fit in the transaction was 28.

#### Execute multiple swap UTxOs for the different trading pairs.

| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.274712 ADA | 0.412068 ADA |
| 2 | 0.330294 ADA | 0.495441 ADA |
| 3 | 0.387914 ADA | 0.581871 ADA |
| 4 | 0.446939 ADA | 0.670409 ADA |
| 5 | 0.506736 ADA | 0.760104 ADA |
| 6 | 0.567834 ADA | 0.851751 ADA |
| 7 | 0.630956 ADA | 0.946434 ADA |
| 8 | 0.694746 ADA | 1.042119 ADA |
| 9 | 0.759940 ADA | 1.139910 ADA |
| 10 | 0.827589 ADA | 1.241384 ADA |
| 15 | 1.186805 ADA | 1.780208 ADA |
| 20 | 1.579676 ADA | 2.369514 ADA |
| 24 | 1.918960 ADA | 2.878440 ADA |

The maximum number of swaps that could fit in the transaction was 24.

## Closing swaps

#### Closing swaps for the same trading pair.

| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.200873 ADA | 0.301310 ADA |
| 10 | 0.281411 ADA | 0.422117 ADA |
| 20 | 0.412048 ADA | 0.618072 ADA |
| 30 | 0.584309 ADA | 0.876464 ADA |
| 40 | 0.797489 ADA | 1.196234 ADA |
| 50 | 1.051766 ADA | 1.577649 ADA |
| 60 | 1.347051 ADA | 2.020577 ADA |
| 70 | 1.683388 ADA | 2.525082 ADA |
| 71 | 1.719279 ADA | 2.578919 ADA |

The maximum number of swaps that could be closed in the transaction was 71.

#### Closing swaps for different trading pairs.

| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.415461 ADA | 0.623192 ADA |
| 10 | 0.435939 ADA | 0.653909 ADA |
| 20 | 0.618936 ADA | 0.928404 ADA |
| 30 | 0.843205 ADA | 1.264808 ADA |
| 40 | 1.108745 ADA | 1.663118 ADA |
| 50 | 1.415338 ADA | 2.123007 ADA |
| 60 | 1.762983 ADA | 2.644475 ADA |
| 70 | 2.151240 ADA | 3.226860 ADA |
| 71 | 2.192279 ADA | 3.288419 ADA |

The maximum number of swaps that could be closed in the transaction was 71.

## Updating swap prices

#### Updating swaps for the same trading pair.

| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.236320 ADA | 0.354480 ADA |
| 5 | 0.423408 ADA | 0.635112 ADA |
| 10 | 0.666614 ADA | 0.999921 ADA |
| 15 | 0.920084 ADA | 1.380126 ADA |
| 20 | 1.183816 ADA | 1.775724 ADA |
| 25 | 1.458031 ADA | 2.187047 ADA |
| 30 | 1.744562 ADA | 2.616843 ADA |
| 33 | 1.921407 ADA | 2.882111 ADA |

The maximum number of swaps that could be updated in the transaction was 33.

#### Updating swaps for different trading pairs.

| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.400755 ADA | 0.601133 ADA |
| 5 | 0.611875 ADA | 0.917813 ADA |
| 10 | 0.885237 ADA | 1.327856 ADA |
| 15 | 1.170031 ADA | 1.755047 ADA |
| 20 | 1.464454 ADA | 2.196681 ADA |
| 22 | 1.585118 ADA | 2.377677 ADA |

The maximum number of swaps that could be updated in the transaction was 22.

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
| 1 | 0.409297 ADA | 0.613946 ADA |
| 5 | 0.657898 ADA | 0.986847 ADA |
| 10 | 0.977651 ADA | 1.466477 ADA |
| 15 | 1.307824 ADA | 1.961736 ADA |
| 16 | 1.375143 ADA | 2.062715 ADA |

The maximum number of swaps that could be updated in the transaction was 16.
