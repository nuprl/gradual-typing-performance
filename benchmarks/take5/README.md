take5
===

Changes
- Fixed num players & num iterations for benchmark
- The `both` folder has type definitions and 1 adaptor.
  The typedefs are there because some rely on the adaptor.



# ORIGINAL README BELOW

## A Object-Oriented Implementation of "Take 5"


Files 
-----

| name     | purpose |
| ----------- | ------------------------------------ |
| README.md   | this file                            |
| 
| main        | the entry point: create players, run |
| utility     | macros for quasi-types               |
| dealer      | manage players, actions, & cards     |
| deck	      | the four visible stacks	   	     |
| stack	      | one stack of cards 		     |
| player      | implement take 6 strategies   	     |
| card-pool   | the complete deck of cards 	     |
| card	      | a card representation 		     |
| basics      | basic constants & types for the game |

Files, Modules and Dependencies 
-------------------------------

The entry point is 
>  main.rkt, which retrieves the desired number of players, creates them,
runs the simulation, and delivers the result;

depends on 
>  dealer.rkt, which constructs a dealer that manages the card deck, the
stacks on the table, and the interactions with the players;

> player.rkt, which implements a player with a specific strategy;

depend on
> card-pool.rkt, which represents the overall deck of cards;
> stack.rkt, which represents one stack of cards on the table; 
> deck.rkt, which represents the stacks on the table;

depend on 
>  card.rkt, a representation of playing cards; 

depends on
>  basics.rkt, which collects basic definitions. 

All files need the module below for the first step towards a gt benchmark
>  utility.rkt, which provides utilities for fake-definitions of opaque and
translucent type exports. 

Running the Program
-------------------

at a shell prompt 
> racket -tm main.rkt n

Testing
-------

> raco test *.rkt 


Reading the Code 
----------------

All files consist of three segments: 

1. an interface, which specifies the exported services;
2. an implementation;
3. and an optional test sub-module. If it exists, it is indicated at the
top of the implementation section with a one-line test sub-module. 

To read any file, open it in DrRacket. We strongly recommend reading the
program from the top down, starting with main. The heart of the program
consists of two modules: dealer and player. 

The dealer module provides an object factory for creating dealers. The only
public method of a dealer object is play-game, which runs one complete game
using the supplied players. 

The player module provides an object factory for creating players. Each
player comes with several public methods, which a dealer object uses to run
rounds and turns. 
