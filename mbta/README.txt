
The implementation uses a non-conventional model/view arrangement. 

 file			purpose 
--------------------------------------------------------------------------------
 my-graph               fix a design flaw in graph/ so that it becomes typable
 t-graph    		model: construct and manage the graph from Source/ data
 t-view    		view: turn queries into probes, answers into responses
 run-t    		the server abstracted over ports, use view (and model)

 stress-test    	stress out run-t

In the typed/ directory: 
-----------------------

 t-graph-types          types for t-graph and t-view 
 t-view-types     	types for t-view and run-t

