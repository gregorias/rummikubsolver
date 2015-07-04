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
to provide current description of the state of the game.

Installation & Usage
======

RummikubSolver uses cabal and standard cabal build order builds the package:

    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build

For now there's only a CLI interface which is run on default.



