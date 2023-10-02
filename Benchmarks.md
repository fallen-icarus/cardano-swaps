# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do all benchmarking tests. All scripts were used as reference scripts to get the best performance possible.

The universal swap spending script requires about 26 ADA to store on-chain.
Each minting policy requires about 18 ADA to be stored on-chain.

## Creating swaps

Each swap requires a mininum UTxO value of about 2 ADA. This is due to the current protocol 
parameters, however, this is desired since it helps prevent denial-of-service attacks for the 
beacon queries.

#### All swaps are for the same trading pair. The offered asset was ADA.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.228296 ADA | 0.342444 ADA |
| 10 | 0.497085 ADA | 0.745628 ADA |
| 20 | 0.795064 ADA | 1.192596 ADA |
| 30 | 1.093132 ADA | 1.639698 ADA |
| 40 | 1.397183 ADA | 2.095775 ADA |
| 50 | 1.695163 ADA | 2.542745 ADA |
| 59 | 1.964127 ADA | 2.946181 ADA | 

59 swaps was the upper limit for this test.

#### All swaps are for the same trading pair. The offered asset was a native token.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.265233 ADA | 0.397850 ADA |
| 10 | 0.569784 ADA | 0.854676 ADA |
| 20 | 0.700861 ADA | 1.051292 ADA |
| 30 | 1.246651 ADA | 1.869977 ADA |
| 40 | 1.591113 ADA | 2.386670 ADA |
| 49 | 1.895840 ADA | 2.842760 ADA |

49 swaps was the upper limit for this test.

#### All swaps are for the same offer asset but different ask assets. The offer asset was a native token.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.268674 ADA | 0.403011 ADA |
| 5 | 0.452232 ADA | 0.678348 ADA |
| 10 | 0.722421 ADA | 1.083632 ADA |
| 15 | 1.000096 ADA | 1.500144 ADA |
| 20 | 1.321653 ADA | 1.982480 ADA |
| 25 | 1.679266 ADA | 2.518899 ADA |
| 29 | 1.975656 ADA | 2.963484 ADA |

29 swaps was the upper limit for this test.

#### All swaps are for the same offer asset but different ask assets. The offer asset is ADA.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.228296 ADA | 0.324444 ADA |
| 5 | 0.386397 ADA | 0.579596 ADA |
| 10 | 0.624765 ADA | 1.937148 ADA |
| 15 | 0.870619 ADA | 1.305929 ADA |
| 20 | 1.160354 ADA | 1.740531 ADA |
| 25 | 1.492042 ADA | 2.238063 ADA |
| 30 | 1.849519 ADA | 2.774279 ADA |

30 swaps was the upper limit for this test.

#### All swaps are for different offer assets but the same ask asset. The ask asset is ADA.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.265233 ADA | 0.397850 ADA |
| 5 | 0.529947 ADA | 0.794921 ADA |
| 10 | 0.988919 ADA | 1.483379 ADA |
| 15 | 1.590641 ADA | 2.385962 ADA |

15 swaps was the upper limit for this test.

#### All swaps are for different offer assets but the same ask asset. The ask asset is a native token.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.268762 ADA | 0.403143 ADA |
| 5 | 0.547593 ADA | 0.821390 ADA |
| 10 | 1.024211 ADA | 1.536317 ADA |
| 15 | 1.643579 ADA | 2.465369 ADA |

15 swaps was the upper limit for this test.

#### All swaps are for different offer assets and different ask assets.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.268762 ADA | 0.403143 ADA |
| 5 | 0.547593 ADA | 0.821390 ADA |
| 10 | 1.024211 ADA | 1.536317 ADA |
| 15 | 1.643579 ADA | 2.465369 ADA |

15 swaps was the upper limit for this test.



## Closing swaps
#### Closing swaps for the same trading pair. The offered asset is a native asset.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.211648 ADA | 0.317472 ADA |
| 10 | 0.353855 ADA | 0.530783 ADA |
| 20 | 0.570432 ADA | 0.855648 ADA |
| 30 | 0.849415 ADA | 1.274123 ADA |
| 40 | 1.190277 ADA | 1.785716 ADA |
| 50 | 1.398190 ADA | 2.097285 ADA | 
| 55 | 1.584887 ADA | 2.377331 ADA | 

The upper limit was 55 swaps closed in a single transaction.

#### Closing swaps for the same trading pair. The offered asset is ADA.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.206887 ADA | 0.310331 ADA |
| 10 | 0.345075 ADA | 0.517613 ADA |
| 20 | 0.557235 ADA | 0.835853 ADA |
| 30 | 0.831758 ADA | 1.247637 ADA |
| 40 | 1.168203 ADA | 1.752305 ADA |
| 50 | 1.566264 ADA | 2.349396 ADA | 
| 56 | 1.596858 ADA | 2.395287 ADA |

The upper limit was 56 swaps closed in a single transaction.

#### Closing swaps for the same offer asset but different ask assets. The offered asset is a native asset.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.360278 ADA | 0.540417 ADA |
| 20 | 0.596892 ADA | 0.895338 ADA |
| 30 | 0.895693 ADA | 1.343540 ADA |
| 40 | 1.256152 ADA | 1.884228 ADA |
| 50 | 1.495494 ADA | 2.243241 ADA |
| 54 | 1.652088 ADA | 2.478132 ADA | 

The upper limit was 54 swaps closed in a single transaction.

#### Closing swaps for the same offer asset but different ask assets. The offered asset is ADA.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.197527 ADA | 0.296291 ADA |
| 10 | 0.353838 ADA | 0.530757 ADA |
| 20 | 0.586035 ADA | 0.879053 ADA |
| 30 | 0.880375 ADA | 1.320563 ADA |
| 40 | 1.236418 ADA | 1.854627 ADA |
| 50 | 1.471343 ADA | 2.207015 ADA |
| 55 | 1.666071 ADA | 2.499107 ADA |

The upper limit was 55 swaps closed in a single transaction.

#### Closing swaps for different offer assets but the same ask asset. The asked asset is ADA.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.458133 ADA | 0.687200 ADA |
| 20 | 0.845325 ADA | 1.267988 ADA |
| 30 | 1.339440 ADA | 2.009160 ADA |
| 38 | 1.699855 ADA | 2.549783 ADA |

The upper limit was 38 swaps closed in a single transaction.

#### Closing swaps for different offer assets but the same ask asset. The asked asset is a native asset.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.458133 ADA | 0.687200 ADA |
| 20 | 0.845325 ADA | 1.267988 ADA |
| 30 | 1.339440 ADA | 2.009160 ADA |
| 38 | 1.699855 ADA | 2.549783 ADA |

The upper limit was 38 swaps closed in a single transaction.

#### Closing swaps for different offer assets and different ask assets.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.458133 ADA | 0.687200 ADA |
| 20 | 0.845325 ADA | 1.267988 ADA |
| 30 | 1.339440 ADA | 2.009160 ADA |
| 38 | 1.699899 ADA | 2.549849 ADA |

The upper limit was 38 swaps closed in a single transaction.



## Updating swap prices
#### Updating swaps for the same trading pair. The offered asset is a native asset.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.228264 ADA | 0.342396 ADA |
| 2 | 0.312946 ADA | 0.469419 ADA |
| 3 | 0.434987 ADA | 0.652481 ADA |
| 4 | 0.599940 ADA | 0.899910 ADA |
| 5 | 0.802253 ADA | 1.203380 ADA |
| 6 | 1.043776 ADA | 1.565664 ADA |
| 7 | 1.320191 ADA | 1.980287 ADA | 

The upper limit was 7 swaps updated in a single transaction.

#### Updating swaps for the same trading pair. The offered asset is ADA.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.223572 ADA | 0.335358 ADA |
| 2 | 0.297959 ADA | 0.446939 ADA |
| 3 | 0.404103 ADA | 0.606155 ADA |
| 4 | 0.545091 ADA | 0.817637 ADA |
| 5 | 0.722773 ADA | 1.084160 ADA |
| 6 | 0.927893 ADA | 1.391840 ADA |
| 7 | 1.174027 ADA | 1.761041 ADA | 
| 8 | 1.450067 ADA | 2.175101 ADA |

The upper limit was 8 swaps updated in a single transaction.

#### Updating swaps for the same offer asset but different ask assets. The offered asset is a native asset.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.228396 ADA | 0.342594 ADA |
| 5 | 0.490379 ADA | 1.735569 ADA |
| 10 | 0.976553 ADA | 1.464830 ADA |
| 14 | 1.508528 ADA | 2.262792 ADA | 

The upper limit was 14 swaps updated in a single transaction.

#### Updating swaps for the same offer asset but different ask assets. The offered asset is ADA.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.22988 ADA | 0.332982 ADA |
| 5 | 0.446417 ADA | 1.669626 ADA |
| 10 | 0.878887 ADA | 1.318331 ADA |
| 15 | 1.469409 ADA | 2.204114 ADA | 
| 16 | 1.574326 ADA | 2.361489 ADA |

The upper limit was 16 swaps updated in a single transaction.

#### Updating swaps for different offer assets but the same ask asset. The asked asset is ADA.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.226063 ADA | 0.339095 ADA |
| 5 | 0.464905 ADA | 0.697358 ADA |
| 10 | 0.896373 ADA | 1.344560 ADA |
| 16 | 1.608552 ADA | 2.412828 ADA | 

The upper limit was 16 swaps updated in a single transaction.

#### Updating swaps for different offer assets but the same ask asset. The asked asset is a native token.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.227867 ADA | 0.341801 ADA |
| 5 | 0.473925 ADA | 0.710888 ADA |
| 10 | 0.914413 ADA | 1.371620 ADA |
| 16 | 1.637944 ADA | 2.456916 ADA | 

The upper limit was 16 swaps updated in a single transaction.

#### Updating swaps for different offer assets and different ask assets.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.227735 ADA | 0.341603 ADA |
| 5 | 0.473265 ADA | 0.709898 ADA |
| 10 | 0.913181 ADA | 1.369772 ADA |
| 16 | 1.636448 ADA | 2.454672 ADA | 

The upper limit was 16 swaps updated in a single transaction.



## Changing what trading pairs swaps are for
#### The swaps started for the same trading pair and ended for the same trading pair. The offer asset remained the same before and after.
| Number Changed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.260155 ADA | 0.390233 ADA |
| 5 | 0.540568 ADA | 0.810852 ADA |
| 10 | 1.055862 ADA | 1.583793 ADA |
| 14 | 1.599920 ADA | 2.399880 ADA | 

The upper limit was 14 swaps updated in a single transaction.

#### The swaps started for the same trading pair and ended for different trading pairs. The offer asset remained the same before and after.
| Number Changed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.260243 ADA | 0.390365 ADA |
| 5 | 0.590810 ADA | 0.886215 ADA |
| 10 | 1.200870 ADA | 1.801305 ADA |
| 13 | 1.638411 ADA | 2.457617 ADA | 

The upper limit was 13 swaps updated in a single transaction.

#### The swaps started for different trading pairs and ended for different trading pairs. 
| Number Changed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.296133 ADA | 0.444200 ADA |
| 5 | 0.751312 ADA | 1.126968 ADA |
| 10 | 1.611050 ADA | 2.416575 ADA |

The upper limit was 10 swaps updated in a single transaction.



## Swap Assets
#### Execute multiple swap UTxOs for the same trading pair and from the same address. The swap change was consolidated into a single output.
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.260583 ADA | 0.390875 ADA |
| 2 | 0.386608 ADA | 0.579912 ADA |
| 3 | 0.572880 ADA | 0.859320 ADA |
| 4 | 0.818312 ADA | 1.228968 ADA |
| 5 | 1.125948 ADA | 1.688922 ADA |

The maximum number of swaps that could fit in the transaction was 5.

#### Execute multiple swap UTxOs for the different trading pairs but from the same address. 
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.260495 ADA | 0.390743 ADA |
| 2 | 0.347506 ADA | 0.521259 ADA |
| 3 | 0.459940 ADA | 0.689910 ADA |
| 4 | 0.561859 ADA | 0.856689 ADA |
| 5 | 0.694843 ADA | 1.042265 ADA |
| 6 | 0.851581 ADA | 1.277372 ADA |
| 7 | 1.001450 ADA | 1.502175 ADA |
| 8 | 1.188749 ADA | 1.782974 ADA |
| 9 | 1.361449 ADA | 2.042174 ADA | 

The maximum number of swaps that could fit in the transaction was 9.

#### Execute multiple swap UTxOs for the same trading pair and from different addresses. 
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.260583 ADA | 0.390875 ADA |
| 2 | 0.353085 ADA | 0.529628 ADA |
| 3 | 0.460125 ADA | 0.690188 ADA |
| 4 | 0.582229 ADA | 0.873344 ADA |
| 5 | 0.720060 ADA | 1.080090 ADA |
| 6 | 0.872384 ADA | 1.308576 ADA |
| 7 | 1.040433 ADA | 1.560650 ADA |
| 8 | 1.222976 ADA | 1.834464 ADA |
| 9 | 1.421244 ADA | 2.131866 ADA | 

The maximum number of swaps that could fit in the transaction was 9.
