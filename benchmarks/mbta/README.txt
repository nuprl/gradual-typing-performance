mbta
====

The implementation uses a non-conventional model/view arrangement. 

 file			purpose 
--------------------------------------------------------------------------------
 stress-test    	stress out run-t; the entry point
 run-t    		the server abstracted over ports, use view (and model)
 t-view    		view: turn queries into probes, answers into responses
 t-graph    		model: construct and manage the graph from Source/ data

In the base/ directory: 
-----------------------
 my-graph               fix a design flaw in graph/ so that it becomes typable
 t-graph-types          types for exporting/importing from/to t-graph and t-view 
 t-view-types     	types for exporting/importing from/to t-view and run-t

 green.dat 		data file 
 blue.dat 		data file 
 red.dat 		data file 
 orange.dat 		data file 

