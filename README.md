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

## Help

Note: the word file option is not actually supported yet.

```
phraser - Generate a passphrase using the diceware method.

Usage: phraser [-v|--version] [-n|--number-of-words NUMBER_OF_WORDS]
               [-s|--seed-integer SEED_INTEGER] [-w|--word-file WORD_FILENAME]
  phraser - Generate a passphrase using the diceware method.

Available options:
  -h,--help                Show this help text
  -v,--version             Show version
  -n,--number-of-words NUMBER_OF_WORDS
                           The number of words to use in the generated
                           passphrase. (default: 10)
  -s,--seed-integer SEED_INTEGER
                           Optional integer for seeding the random generator.
  -w,--word-file WORD_FILENAME
                           Optional file for populating the word map.
```
