# phraser

## Description

Generate a passphrase using the diceware method.
See http://world.std.com/~reinhold/diceware.html

I wrote this utility to practice using
[MTL](https://hackage.haskell.org/package/mtl),
[cryptonite](https://hackage.haskell.org/package/cryptonite),
and [Haddock](https://www.haskell.org/haddock).
So hopefully it's a simple demonstration of those things.

## Install

The simplest way is to use [stack](https://haskellstack.org).

```
git clone https://github.com/srdqty/phraser.git
cd phraser
stack install --local-bin-path $HOME/.local/bin
```
