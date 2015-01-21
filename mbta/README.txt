
The implementation uses a non-conventional model/view arrangement. 

 file			purpose 
--------------------------------------------------------------------------------
 t-graph.rkt		model: construct and manage the graph from Source/ data
 t.rkt			view: turn queries into probes, answers into responses
 run-t.rkt		the server abstracted over ports, use view (and model)

 stress-test.rkt	stress out run-t

 xrun-via-tcp		a TCP-based server, using run-t via network ports
 tcp-t.rkt		a TCP-based probe: run server via repl 
 tcp-constants.rkt	constants for TCP services
