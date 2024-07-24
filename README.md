A Duck-themed MineSweeper made using Haskell 

Team: Kriti Chaturvedi
      Mariam Eqbal
      Ritigya Gupta

# Work Division
 Mariam - Basic logic of the Minesweeper board and implementation in the terminal (without GUI)\
 Ritigya - Making an attractive Haskell GUI\
 Kriti - Testing and debugging


# Clone and play on your systems!
* Clone the git repository
* Run the following commands on your terminal
```
cd ducking_off
sudo apt update
sudo apt install libgtk2.0-0 libgtk2.0-dev libgmp-dev
cabal update
cabal install gtk2hs-buildtools
cabal install gtk
``` 
* For the GUI version

``` 
cabal build
cabal run
```

* For the CLI version
```
cd app
runghc mine.hs
```

***
