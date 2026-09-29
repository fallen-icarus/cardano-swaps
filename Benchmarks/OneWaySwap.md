# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do 
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

- The universal swap spending script requires 15.873730 ADA to store on-chain.
- The universal minting policy requires 18.567480 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### All swaps are for the same trading pair. The trading pair was (native asset,ADA).

| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.232576 ADA | 0.348864 ADA |
| 10 | 0.591435 ADA | 0.887153 ADA |
| 20 | 0.990167 ADA | 1.485251 ADA |
| 30 | 1.389075 ADA | 2.083613 ADA |
| 34 | 1.548568 ADA | 2.322852 ADA |

The maximum number of swaps that could be created was 34.

#### All swaps are for different trading pairs.

| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.286043 ADA | 0.429065 ADA |
| 10 | 0.700176 ADA | 1.050264 ADA |
| 20 | 1.160841 ADA | 1.741262 ADA |
| 24 | 1.345521 ADA | 2.018282 ADA |

The maximum number of swaps that could be created was 24.

## Swap Assets

Swaps are validated by checking each output in the transaction. The checks are essentially:

1) Does this output have the beacon from the input?
2) If "Yes" to (1), is this output locked at the address where the input comes from?
3) If "Yes" to (2), does this output have the proper datum for the corresponding output?

#### Execute multiple swap UTxOs for the same trading pair and from the same address.

| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.227704 ADA | 0.341556 ADA |
| 2 | 0.277283 ADA | 0.415925 ADA |
| 3 | 0.328233 ADA | 0.492350 ADA |
| 4 | 0.380552 ADA | 0.570828 ADA |
| 5 | 0.434285 ADA | 0.651428 ADA |
| 6 | 0.489344 ADA | 0.734016 ADA |
| 7 | 0.545773 ADA | 0.818660 ADA |
| 8 | 0.603571 ADA | 0.905357 ADA |
| 9 | 0.662740 ADA | 0.994110 ADA |
| 10 | 0.723278 ADA | 1.084917 ADA |
| 15 | 1.046514 ADA | 1.569771 ADA |
| 20 | 1.403994 ADA | 2.105991 ADA |
| 25 | 1.795982 ADA | 2.693973 ADA |
| 29 | 2.134193 ADA | 3.201290 ADA |

The maximum number of swaps that could fit in the transaction was 29.

#### Execute multiple swap UTxOs for the different trading pairs.

| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.270705 ADA | 0.406058 ADA |
| 2 | 0.323727 ADA | 0.485591 ADA |
| 3 | 0.378128 ADA | 0.567192 ADA |
| 4 | 0.433063 ADA | 0.649595 ADA |
| 5 | 0.490323 ADA | 0.735485 ADA |
| 6 | 0.548856 ADA | 0.823284 ADA |
| 7 | 0.608663 ADA | 0.912995 ADA |
| 8 | 0.669846 ADA | 1.004769 ADA |
| 9 | 0.732512 ADA | 1.098768 ADA |
| 10 | 0.796660 ADA | 1.194990 ADA |
| 15 | 1.136491 ADA | 1.704737 ADA |
| 20 | 1.509596 ADA | 2.264394 ADA |
| 24 | 1.835382 ADA | 2.753073 ADA |

The maximum number of swaps that could fit in the transaction was 24.

## Closing swaps

#### Closing swaps for the same trading pair.

| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.200585 ADA | 0.300878 ADA |
| 10 | 0.279601 ADA | 0.419402 ADA |
| 20 | 0.408646 ADA | 0.612969 ADA |
| 30 | 0.579314 ADA | 0.868971 ADA |
| 40 | 0.790902 ADA | 1.186353 ADA |
| 50 | 1.043587 ADA | 1.565381 ADA |
| 60 | 1.337279 ADA | 2.005919 ADA |
| 70 | 1.672023 ADA | 2.508035 ADA |
| 72 | 1.743898 ADA | 2.615847 ADA |

The maximum number of swaps that could be closed in the transaction was 72.

#### Closing swaps for different trading pairs.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.415173 ADA | 0.622760 ADA |
| 10 | 0.434129 ADA | 0.651194 ADA |
| 20 | 0.615534 ADA | 0.923301 ADA |
| 30 | 0.838386 ADA | 1.257579 ADA |
| 40 | 1.102158 ADA | 1.653237 ADA |
| 50 | 1.406939 ADA | 2.110409 ADA |
| 60 | 1.752551 ADA | 2.628827 ADA |
| 70 | 2.139655 ADA | 3.209483 ADA |
| 72 | 2.222002 ADA | 3.333003 ADA |

The maximum number of swaps that could be closed in the transaction was 72.

## Updating swap prices

#### Updating swaps for the same trading pair.

| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.237285 ADA | 0.355928 ADA |
| 5 | 0.428782 ADA | 0.643173 ADA |
| 10 | 0.677610 ADA | 1.016415 ADA |
| 15 | 0.936701 ADA | 1.405052 ADA |
| 20 | 1.206055 ADA | 1.809083 ADA |
| 25 | 1.485891 ADA | 2.228837 ADA |
| 30 | 1.775991 ADA | 2.663987 ADA |
| 31 | 1.835243 ADA | 2.752865 ADA |

The maximum number of swaps that could be updated in the transaction was 31.

#### Updating swaps for different trading pairs.

| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.397039 ADA | 0.595559 ADA |
| 5 | 0.594516 ADA | 0.891774 ADA |
| 10 | 0.852177 ADA | 1.278266 ADA |
| 15 | 1.119804 ADA | 1.679706 ADA |
| 20 | 1.397273 ADA | 2.095910 ADA |
| 22 | 1.511229 ADA | 2.266844 ADA |

The maximum number of swaps that could be updated in the transaction was 22.

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
| 1 | 0.327379 ADA | 0.491069 ADA |
| 5 | 0.562213 ADA | 0.843320 ADA |
| 10 | 0.865484 ADA | 1.298226 ADA |
| 15 | 1.178914 ADA | 1.768371 ADA |
| 18 | 1.371382 ADA | 2.057073 ADA |

The maximum number of swaps that could be updated in the transaction was 18.
