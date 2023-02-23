# catmap

catroidvanias scuffed dungeon map thing

using haskell with gloss

requires:

- cabal
- ghc

build with:
```
cabal build
```

run with:
```
_[path to executable]_ _[map file]_
```

expects maps like this:

- mapName.txt as:
```
wwww
woow
woow
wwww
```

- .bmp image files for each character eg.
- w.bmp
- o.bmp
- ...
