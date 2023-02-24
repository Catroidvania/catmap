# catmap

catroidvanias scuffed dungeon map thing

using haskell with gloss

### building

requires:

- cabal
- ghc

build with:
```
cabal build
```

run with:
```
[path to executable] [mapName.txt]
```

### map format

expects maps like this:

- mapName.txt as:
```
wwww
woow
woow
wwww
```

- .bmp image files for each character in the map file in a folder called `textures`
- w.bmp
- o.bmp
- ...

- .bmp files for icons per letter in a folder called `icons`
- a.bmp
- b.bmp
- ...

### controls

movement:
- w a s d / arrow keys : move map
- shift + wasd/arrow keys : fast move
- q e : shrink / enlarge map in freelook mode
- left click : jump cursor to clicked position
- r : return cursor to 0 0

toggles:
- t : show / hide fog
- f : enter / exit freelook mode
- g : show / hide grid
- space bar : toggle tile under cursor visibility

tints:
- 1 : red
- 2 : orange
- 3 : yellow
- 4 : green
- 5 : blue
- 6 : magenta
- 7 : white
- 8 : grey
- 9 : black
- 0 : none
- shift + color : lighter color
- ctrl + color : darker color

icons:
- alt + letter : put icon letter.bmp on tile under cursor (ex. alt+z puts z.bmp if it exists onto the current tile)
- backspace / delete : remove top icon from tile under cursor

### else

go nuts

catroidvania out
