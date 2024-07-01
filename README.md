# Ducking Off
## An implementation of Minesweeper in Haskell

# Timeline
WEEK 1:\
    * Make test cases (3 boards for testing at different stages)\
    * Make the board from scratch\
    * Build GUI\
    * Integrate all components\

WEEK 2:\
    * Days 1 - 3: Finalising the code, structuring, ordering and completing code.\
    * Days 4 - 7: Final testing and debugging. \

# Work Division
 Mariam - Basic logic of the Minesweeper board and implementation in the terminal (without GUI)\
 Ritigya - Making an attractive Haskell GUI\
 Kriti - Testing and debugging\


# Our cute modifications

Unopened tiles will be respresented by lilypads.\
Numbered tiles will be represented by water.\
Mines will be represented by sharks.\
Zero numbered tiles will be represented by ducks.\
Flags will be represented by lilies.\

# Gtk2Hs Setup Commands on terminal

* sudo apt update
* sudo apt install libgtk2.0-0 libgtk2.0-dev libgmp-dev
* cabal update
* cabal install gtk2hs-buildtools
* cabal install gtk
* setup your Haskell project (if not already) then do changes to your .cabal file -
  build-depends:  base >=4.7 && <5,    
                  gtk >=0.15 && <0.16
* create hs file which has gui code (here ex- initGUI.hs)
* outside your project-
  * cabal build
  * cabal run

***
