# Which

Implements the classical "which" command. Behavior is close to the real thing, but not exactly the same.

To make testing easy, a Reader monad was used, to supply IO operations in a context. 
This makes it easy to provide fake implementations (see tests).

Both HSpec and QuickCheck was used for testing.

Idea from https://codingchallenges.substack.com/p/coding-challenge-93-which

## Run

```
nix develop
cabal test
cabal run . -- ls which env
```
