---
Number: 1
Title: No Yield Farming
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

It is important to recognize why yield farming is used. When Alice provides liquidity to a liquidity
pool, she is agreeing to swap that asset at any price. This is great if the price changes in Alice's
favor but this is terrible if the price moves against her. In bull markets, it is better to hold the
asset but this is also the time when Bob will come along and swap for that asset. In short, Alice is
agreeing to potentially give up the asset when it is in her best interest to hold the asset. This
risk is called *impermanent loss* and exists for all liquidity pools. By itself, this would deter
people and businesses from agreeing to provide liquidity for the dApp. Therefore, to cancel out the
risk of impermanent loss, dApps agree to pay users for providing liquidity. This is where yield
farming comes in. dApps don't actually have a sustainable pot from which to pay liquidity providers
and so they mint a token to use as payments. **This is red flag #1.**

Yield farming is worse than when a country prints money to pay its bills. When a country prints
money, that money can be used throughout that country's economy - Alice can use it to buy food, pay
rent, etc. The same is true for Cardano paying rewards from the reserve pot - ADA can be used to pay
every transaction fee and therefore can be used with every dApp. When a dApp mints a token (aka
prints money), what can Alice do with the new tokens?
1. Stake it to earn more of that token.
2. Vote on proposals for that dApp.
3. Hold the token to unlock cheaper dApp fees.
4. Hold the token to unlock premium dApp features.
5. Sell the token. 

In other words, Alice is extremely limited in what she can do with these new tokens. So limited in
fact, that the tokens don't seem to be a currency at all. Instead, these tokens act more like VIP
tokens. So for DEXs, liquidity providers put up their currencies and experience impermanent loss. To
help mitigate this loss, they are given VIP tokens. **This is red flag #2.**

Now imagine the scenario where Alice provided liquidity and lost $1000 to impermanent loss. She
received some yield tokens to help offset the loss. Ultimately, Alice does not want the yield tokens
since they are very limited in their usecases and would rather have her $1000 back. So she looks to
sell the tokens to Bob. What incentive does Bob have to buy the tokens from Alice?

There are only two reasons for someone to buy an asset: they believe they can resell it at a higher
price at a later date or they need to use the asset. As already stated, these tokens don't have any
real economic use - they aren't even used to pay fees on the dApp. So the only other option is if
the tokens can be resold at a higher price. Is this a realistic belief? Well, again, these tokens
don't have any real utility in the economy, meanwhile the circulating supply is being increased
every time yield farming pays out. So actually, the value of these tokens would decrease over time.

"Hold on! Some dApps are not going to run yield farming forever! Therefore, the tokens won't
actually go to zero!" Stopping yield farming would be bad, too. Remember that yield farming exists
to incentivize liquidity providers to accept the risk of impermanent loss. No yield farming means
there is no incentive to accept the risk of impermanent loss. No incentive to accept the risk means
no liquidity for liquidity pools. The dApp would effectively collapse.

So if yield farming ever stops, liquidity pools would collapse. But if yield farming doesn't stop,
the tokens being used to cover the losses from impermanent loss will have their value go to zero. So
ask yourself again, what incentive does Bob have to buy the tokens from Alice? He doesn't have
one. Without someone willing to buy the tokens from Alice, she will be stuck with the tokens and be
unable to recoup her $1000. **This is red flag #3.**

"If yield farming is just a house of cards, why hasn't the whole thing collapsed yet?!" Good
question! The reason is because Alice hasn't tried to sell her tokens yet. She has restaked them
(usecase #1) so that she can earn even more in the next yield farming pay out. This system will work
as long as users don't try to cash out. But again, 1) people are accepting these yield tokens to
cover the losses from impermanent loss and 2) the value of these tokens will only decrease as more
are minted which means users will recover less of their losses the longer they wait to cash out.
Eventually, users will cash out. But now go back to the question for Bob: who is going to buy the
tokens from the users that now want to cash out?

Yield farming is a currency crisis waiting to happen...

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Cardano-Swaps will not use yield farming to incentivize the build up of liquidity. Instead,
liquidity will build up organically from users simply wanting to use the DEX.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

Liquidity for Cardano-Swaps will be slower to build up however, any liquidity that does build up
will be far more stable than if it was arificially incentivized by yield farming. Considering that
this DEX is meant to act as a foundation for a decentralized economy, long term stability should be
prioritized over immediate liquidity.
