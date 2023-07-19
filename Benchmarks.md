# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do all benchmarking tests. All scripts were used as reference scripts to get the best performance possible.

The universal swap spending script requires about 26 ADA to store on-chain.
Each minting policy requires about 18 ADA to be stored on-chain.

## Creating Swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol parameters, however, this is desired since it helps prevent denial-of-service attacks for the beacon queries.

### All swaps are for the same trading pair.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.218781 ADA | 0.328172 ADA |
| 10 | 0.444279 ADA | 0.666419 ADA |
| 20 | 0.700861 ADA | 1.051292 ADA |
| 30 | 0.951504 ADA | 1.427256 ADA |
| 40 | 1.208086 ADA | 1.812129 ADA |
| 50 | 1.464668 ADA | 2.197002 ADA |

59 swaps was the upper limit for this test.

### All swaps are for the same offer asset but different ask assets.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.218781 ADA | 0.328172 ADA |
| 10 | 0.567442 ADA | 0.851163 ADA |
| 20 | 1.074735 ADA | 1.612104 ADA |
| 30 | 1.743686 ADA | 2.615529 ADA |

The max number of swaps that could be created in a single transaction was 31.

### All swaps are for different offer assets and different ask assets.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.218781 ADA | 0.328172 ADA |
| 5 | 0.511252 ADA | 0.766878 ADA |
| 10 | 0.965999 ADA | 1.448999 ADA |
| 15 | 1.569976 ADA | 2.354964 ADA |

Maximum number of swaps that could be created in a single transaction was 16.

## Swap Assets

### Composing swaps of different trading pairs.

The first test was just a single swap in isolation. The swap change was combined into a single output.

| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.273922 ADA | 0.410883 ADA |
| 2 | 0.371632 ADA | 0.557448 ADA |
| 3 | 0.497771 ADA | 0.719657 ADA |
| 4 | 0.600349 ADA | 0.900524 ADA |
| 5 | 0.735377 ADA | 1.103066 ADA |
| 6 | 0.880833 ADA | 1.321250 ADA |
| 7 | 1.038729 ADA | 1.558094 ADA |
| 8 | 1.209064 ADA | 1.813596 ADA |
| 9 | 1.391838 ADA | 2.087757 ADA |

10 swaps composed together exceeded the transaction limits.

### Aggregating swaps for the same trading pair from the same address.

| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.241081 ADA | 0.361622 ADA |
| 2 | 0.343378 ADA | 0.515067 ADA |
| 3 | 0.493786 ADA | 0.740679 ADA |
| 4 | 0.692288 ADA | 1.038432 ADA |
| 5 | 0.938884 ADA | 1.408326 ADA |
| 6 | 1.233574 ADA | 1.850361 ADA |

Aggregating 7 swaps exceeded the transaction limits.

## Closing Swaps

### Closing swaps for the same trading pair.

| Number of Swaps Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198758 ADA | 0.298137 ADA |
| 10 | 0.347376 ADA | 0.521064 ADA |
| 20 | 0.575358 ADA | 0.863037 ADA |
| 30 | 0.870160 ADA | 1.305240 ADA |
| 40 | 1.231342 ADA | 1.847013 ADA |

The upper limit was 47 swaps closed in a single transaction.

### Closing swaps for the same offer asset but different ask assets.

| Number of Swaps Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198758 ADA | 0.298137 ADA |
| 10 | 0.366300 ADA | 0.549450 ADA |
| 20 | 0.615211 ADA | 0.922817 ADA |
| 30 | 0.930942 ADA | 1.396413 ADA |
| 40 | 1.312612 ADA | 1.968918 ADA |

The upper limit was 47 swaps closed in a single transaction.

### Closing swaps for different offer assets.

| Number of Swaps Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.198758 ADA | 0.298137 ADA |
| 10 | 0.473230 ADA | 0.709845 ADA |
| 20 | 0.883480 ADA | 1.325220 ADA |
| 30 | 1.406943 ADA | 2.110415 ADA |

The upper limit was about 35 swaps closed in a single transaction.

## Updating Swaps

For all tests, the number of swap outputs equalled the number of swap inputs.

### Updating swaps for the same trading pair.

| Number of Swaps Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.218081 ADA | 0.327122 ADA |
| 2 | 0.280067 ADA | 0.420101 ADA |
| 3 | 0.367078 ADA | 0.550617 ADA |
| 4 | 0.479117 ADA | 0.718676 ADA |
| 5 | 0.616181 ADA | 0.924272 ADA |
| 6 | 0.778272 ADA | 1.167408 ADA |
| 7 | 0.965389 ADA | 1.448084 ADA |
| 8 | 1.177532 ADA | 1.766298 ADA |
| 9 | 1.414701 ADA | 2.122052 ADA |

The maximum number of swaps that could be updated in a single transaction was 9.

### Updating swaps for the same offer asset but different ask assets.

| Number of Swaps Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.218743 ADA | 0.328115 ADA |
| 5 | 0.434707 ADA | 0.652061 ADA |
| 10 | 0.853623 ADA | 1.280435 ADA |
| 15 | 1.438176 ADA | 2.157264 ADA |

The maximum number of swaps that could be updated in a single transaction was 16.

### Updating swaps for different offer assets.

| Number of Swaps Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.218081 ADA | 0.327122 ADA |
| 5 | 0.443259 ADA | 0.664889 ADA |
| 10 | 0.863708 ADA | 1.295562 ADA |
| 15 | 1.439145 ADA | 2.158718 ADA |

The maximum number of swaps that could be updated in a single transaction was 16.