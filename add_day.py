#!/usr/bin/python3.8
import itertools

fh = open("adventofcode2017.cabal", "a")

for day, sect in itertools.product(range(1, 26), range(1,3)):
        s = f"""
executable {day}-{sect}
    main-is: days/{day}-{sect}.hs
    default-language: Haskell2010
    build-depends:
        base,
        adventofcode2017
"""
        fh.write(s)


