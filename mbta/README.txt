
The implementation uses a non-conventional model/view arrangement. 

 file			purpose 
--------------------------------------------------------------------------------
 t-graph.rkt		model: construct and manage the graph from Source/ data
 t-view.rkt		view: turn queries into probes, answers into responses
 run-t.rkt		the server abstracted over ports, use view (and model)

 stress-test.rkt	stress out run-t

In the typed/ directory: 
-----------------------

 t-graph-types.rkt      types for t-graph and t-view 
 t-view-types.rkt 	types for t-view and run-t

