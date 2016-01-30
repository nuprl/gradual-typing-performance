================================================================================
This directory contains the Racket code base for the variant of the Acquire
board game that was used as the theme of "Software Construction" at NU PRL
in the Spring 2013 semester. 

================================================================================
client-start: launch a named remote client for an Acquire game 
sever-start:  launch a server for an Acquire game 

remote-game: run tests for the remote version of the game 
tree-game:   run tests for the local version of the game 

================================================================================
The remaining files relate as follows to each other: 
    
external players, potentially supplied by others 
 
   player      |           |  strategy  
   player-intf |  protocol |  strategy-intf
      |                         |                    
      v                         V                    
      +-------------------------+
                     |
               player-factory
                     |
                     v
===============================================================================
                     |
                     |
                     v
                  tree-game || tree-game-intf <==== entry point to play

		  protocol governs interaction in principle 

   --- remote-proxy layer exists here ---
   --- remote-actor[intf], remote-admin, remote-player ---

		  protocol governs interaction in principle 

                            || game-intf      <==== entry point to play
                     ^
                     |
                     |
===============================================================================
                     |
                     |
      +------------------+------------------+--------------...
      ^                  ^                  ^           ^
      |                  |                  |		|
      |                  |                  |		|
   state-intf       board-intf         basics-intf     tree-intf
   state            board              basics 	       tree

================================================================================
basic libraries (unrelated to game): 

  Lib/ 
     auxiliaries -- basic library functions 
     contract    -- contracts, should be in unstable/contract 
     io          -- some I/O functions 
     log         -- central place for logging exns 
     sandbox     -- wire up 'with-limit' in a function 
     struct      -- a facility for opening up structs locally 
     xml         -- XML parsing utilities 

  Performance/ 
     with-contract -- a file that governs whether contracts should be checked
     		   [This is for Christos D's research project.]

todo: 
 
 * the choice of tile should be computed once and for all not every time 
   a tile is transferred from the admin to a player 

potential bug: 

 * protocols occasionally seems to use state-place-tile on player that 
   doesn't have the tile (merger perhaps? -- not reproducible)
