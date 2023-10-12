# Benchmarks (YMMV)

The node emulator from [plutus-apps](https://github.com/input-output-hk/plutus-apps) was used to do
all benchmarking tests. All scripts were used as reference scripts to get the best performance 
possible.

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
| 58 | 1.968027 ADA | 2.952041 ADA | 

58 swaps was the upper limit for this test.

#### All swaps are for the same trading pair. The offered asset was a native token.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.265233 ADA | 0.397850 ADA |
| 10 | 0.569784 ADA | 0.854676 ADA |
| 20 | 0.700861 ADA | 1.051292 ADA |
| 30 | 1.246651 ADA | 1.869977 ADA |
| 40 | 1.591113 ADA | 2.386670 ADA |
| 48 | 1.893118 ADA | 2.839677 ADA |

48 swaps was the upper limit for this test.

#### All swaps are for the same offer asset but different ask assets. The offer asset was a native token.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.268674 ADA | 0.403011 ADA |
| 5 | 0.452232 ADA | 0.678348 ADA |
| 10 | 0.722421 ADA | 1.083632 ADA |
| 15 | 1.000096 ADA | 1.500144 ADA |
| 20 | 1.321653 ADA | 1.982480 ADA |
| 25 | 1.679266 ADA | 2.518899 ADA |
| 28 | 1.923147 ADA | 2.884736 ADA |

28 swaps was the upper limit for this test.

#### All swaps are for the same offer asset but different ask assets. The offer asset is ADA.
| Number of Swaps Created | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.228296 ADA | 0.324444 ADA |
| 5 | 0.386397 ADA | 0.579596 ADA |
| 10 | 0.624765 ADA | 1.937148 ADA |
| 15 | 0.870619 ADA | 1.305929 ADA |
| 20 | 1.160354 ADA | 1.740531 ADA |
| 25 | 1.492042 ADA | 2.238063 ADA |
| 29 | 1.796413 ADA | 2.694620 ADA |

29 swaps was the upper limit for this test.

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
| 1 | 0.242436 ADA | 0.363654 ADA |
| 2 | 0.309928 ADA | 0.464892 ADA |
| 3 | 0.385710 ADA | 0.578565 ADA |
| 4 | 0.469694 ADA | 0.704541 ADA |
| 5 | 0.561925 ADA | 0.842888 ADA |
| 6 | 0.662402 ADA | 0.993603 ADA |
| 7 | 0.771124 ADA | 1.156686 ADA |
| 8 | 0.888093 ADA | 1.332140 ADA |
| 9 | 1.013308 ADA | 1.519962 ADA |
| 10 | 1.152841 ADA | 1.729262 ADA |
| 11 | 1.294548 ADA | 1.941822 ADA |
| 12 | 1.444502 ADA | 2.166753 ADA |

The maximum number of swaps that could fit in the transaction was 12.

#### BEST CASE SCENARIO: Execute multiple swap UTxOs for the different trading pairs. 
| Number of Swaps | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.248778 ADA | 0.373167 ADA |
| 2 | 0.318082 ADA | 0.477123 ADA |
| 3 | 0.392362 ADA | 0.588543 ADA |
| 4 | 0.469743 ADA | 0.704615 ADA |
| 5 | 0.556761 ADA | 0.835142 ADA |
| 6 | 0.638943 ADA | 0.958415 ADA |
| 7 | 0.724225 ADA | 1.086338 ADA |
| 8 | 0.814483 ADA | 1.221725 ADA |
| 9 | 0.916287 ADA | 1.374431 ADA | 
| 10 | 1.011478 ADA | 1.517217 ADA |
| 11 | 1.109769 ADA | 1.664654 ADA |
| 12 | 1.211161 ADA | 1.816742 ADA |
| 13 | 1.321680 ADA | 1.982520 ADA |
| 14 | 1.429271 ADA | 2.143907 ADA |
| 15 | 1.539962 ADA | 2.309943 ADA |
| 16 | 1.655630 ADA | 2.483445 ADA | 

The maximum number of swaps that could fit in the transaction was 16.



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
| 53 | 1.560789 ADA | 2.341184 ADA | 

The upper limit was 53 swaps closed in a single transaction.

#### Closing swaps for the same trading pair. The offered asset is ADA.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.206887 ADA | 0.310331 ADA |
| 10 | 0.345075 ADA | 0.517613 ADA |
| 20 | 0.557235 ADA | 0.835853 ADA |
| 30 | 0.831758 ADA | 1.247637 ADA |
| 40 | 1.168203 ADA | 1.752305 ADA |
| 50 | 1.566264 ADA | 2.349396 ADA | 
| 54 | 1.573670 ADA | 2.360505 ADA |

The upper limit was 54 swaps closed in a single transaction.

#### Closing swaps for the same offer asset but different ask assets. The offered asset is a native asset.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.360278 ADA | 0.540417 ADA |
| 20 | 0.596892 ADA | 0.895338 ADA |
| 30 | 0.895693 ADA | 1.343540 ADA |
| 40 | 1.256152 ADA | 1.884228 ADA |
| 50 | 1.495494 ADA | 2.243241 ADA |
| 53 | 1.641421 ADA | 2.462132 ADA | 

The upper limit was 53 swaps closed in a single transaction.

#### Closing swaps for the same offer asset but different ask assets. The offered asset is ADA.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.197527 ADA | 0.296291 ADA |
| 10 | 0.353838 ADA | 0.530757 ADA |
| 20 | 0.586035 ADA | 0.879053 ADA |
| 30 | 0.880375 ADA | 1.320563 ADA |
| 40 | 1.236418 ADA | 1.854627 ADA |
| 50 | 1.471343 ADA | 2.207015 ADA |
| 53 | 1.655919 ADA | 2.483879 ADA |

The upper limit was 53 swaps closed in a single transaction.

#### Closing swaps for different offer assets but the same ask asset. The asked asset is ADA.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.458133 ADA | 0.687200 ADA |
| 20 | 0.845325 ADA | 1.267988 ADA |
| 30 | 1.339440 ADA | 2.009160 ADA |
| 37 | 1.699855 ADA | 2.549783 ADA |

The upper limit was 37 swaps closed in a single transaction.

#### Closing swaps for different offer assets but the same ask asset. The asked asset is a native asset.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.458133 ADA | 0.687200 ADA |
| 20 | 0.845325 ADA | 1.267988 ADA |
| 30 | 1.339440 ADA | 2.009160 ADA |
| 37 | 1.663178 ADA | 2.494767 ADA |

The upper limit was 37 swaps closed in a single transaction.

#### Closing swaps for different offer assets and different ask assets.
| Number Closed | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.199949 ADA | 0.299924 ADA |
| 10 | 0.458133 ADA | 0.687200 ADA |
| 20 | 0.845325 ADA | 1.267988 ADA |
| 30 | 1.339440 ADA | 2.009160 ADA |
| 37 | 1.663090 ADA | 2.494635 ADA |

The upper limit was 37 swaps closed in a single transaction.



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

The upper limit was 7 swaps updated in a single transaction.

#### Updating swaps for the same offer asset but different ask assets. The offered asset is a native asset.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.228396 ADA | 0.342594 ADA |
| 5 | 0.490379 ADA | 1.735569 ADA |
| 10 | 0.976553 ADA | 1.464830 ADA |
| 15 | 1.552419 ADA | 2.328629 ADA | 

The upper limit was 15 swaps updated in a single transaction.

#### Updating swaps for the same offer asset but different ask assets. The offered asset is ADA.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.22988 ADA | 0.332982 ADA |
| 5 | 0.446417 ADA | 1.669626 ADA |
| 10 | 0.878887 ADA | 1.318331 ADA |
| 15 | 1.469409 ADA | 2.204114 ADA | 

The upper limit was 15 swaps updated in a single transaction.

#### Updating swaps for different offer assets but the same ask asset. The asked asset is ADA.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.226063 ADA | 0.339095 ADA |
| 5 | 0.464905 ADA | 0.697358 ADA |
| 10 | 0.896373 ADA | 1.344560 ADA |
| 15 | 1.608552 ADA | 2.412828 ADA | 

The upper limit was 15 swaps updated in a single transaction.

#### Updating swaps for different offer assets but the same ask asset. The asked asset is a native token.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.227867 ADA | 0.341801 ADA |
| 5 | 0.473925 ADA | 0.710888 ADA |
| 10 | 0.914413 ADA | 1.371620 ADA |
| 15 | 1.637944 ADA | 2.456916 ADA | 

The upper limit was 15 swaps updated in a single transaction.

#### Updating swaps for different offer assets and different ask assets.
| Number Updated | Tx Fee | Collateral Required |
|:--:|:--:|:--:|
| 1 | 0.227735 ADA | 0.341603 ADA |
| 5 | 0.473265 ADA | 0.709898 ADA |
| 10 | 0.913181 ADA | 1.369772 ADA |
| 15 | 1.636448 ADA | 2.454672 ADA | 

The upper limit was 15 swaps updated in a single transaction.



## Changing what trading pairs swaps are for
By composing both the `CreateSwap` minting redeemer and the `CloseOrUpdate` spending redeemer, it
is possible change what trading pair a swap is for in a single transaction (ie, you do not need to
first close the swap in one tx and than open the new swap in another tx).

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
