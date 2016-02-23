mbta+graph
==========

Nearly self-contained MBTA benchmark.
There are a few requires from Racket's `set` and syntax libraries, but
the important modules from the graph library are included in the experiment.


The implementation uses a non-conventional model/view arrangement. 

 file			purpose 
--------------------------------------------------------------------------------
 main    		stress out run-t; the entry point
 run-t    		the server abstracted over ports, use view (and model)
 t-view    		view: turn queries into probes, answers into responses
 t-graph    		model: construct and manage the graph from Source/ data
 graph-adjlist-utils	from Stephen's library, helpers for working with adjaceny lists
 graph-attach-edge-property	fixes a design flaw in the graph library
 graph-struct	data definition for graphs
 graph-unweighted	implementation of an unweighted, directed graph

In the base/ directory: 
-----------------------
 green.dat 		data file 
 blue.dat 		data file 
 red.dat 		data file 
 orange.dat 		data file 

In the both/ directory:
-----------------------
 graph-types.rkt		typed adapter, contains all types used by the graph and mbta modules
