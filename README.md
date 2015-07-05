Overview
========

A solver for Rummikub game.

Description
===========

Don't know whether there is a winning move? Someone can't finish, has made a
mess on board, and can't retrieve original sets? Don't worry! RummikubSolver
automatically finds the solution.

RummikubSolver uses integer linear programming to find the solution which
maximizes the number of tiles placed on the table from the rack. You just need
to provide current description of the state of the game. RummikubSolver will
display possible sets that are achievable and tiles that may be placed to
achieve them.

Installation & Usage
======

RummikubSolver uses cabal and standard cabal build order builds the package:

    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build

This places the executable file in
<code>dist/build/RummikubSolver/RummikubSolver</code>.

RummikubSolver has 2 UIs available: CLI and GUI. GUI is turn on on default and
uses threepenny-gui to run a web interface. Type <code>127.0.0.1:8080</code> to
use the GUI.

Both interfaces use special syntax for add or removing tiles from the table or
the rack. For example:

    -lr1-3 // Remove (-) blue (l) and red (r) tiles of value from 1 to 3.
    y10 // Add a yellow (y) 10 tile
    j // Add a joker (j)
    -j, b2 // Remove a joker (j) and add black (b) 2.

Formally the queries have following syntax:
    
    QUERY ::= [-] (COLOR+ (VALUE | VALUE_RANGE) | j)
    COLOR ::= [rlyb]
    VALUE ::= 1-13
    VALUE_RANGE ::= VALUE - VALUE

Final remarks
=====

This program was done to train my Rummikub skills and as a helping tool for
board game nights.

TODO
=====

* Check whether fresh build works and add remarks if necessary
* Add unit tests
* Move tile parsing to one file

