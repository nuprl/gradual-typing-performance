
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


=============================================================================

Ran a little experiment profiling runs.
Results suggest that typed configurations are faster because we spend
less time creating threads.

Regardless, the program seems to have a bug (spawning too many threads)

Typed:
------
Num. samples: 100
(Runtime)
Average running time: 5571.99
Median running time: 4021.0
Min running time: 2508
Max running time: 13452
95% confidence: 4944.41 -- 6199.57
(Thread Count)
Average num. threads observed: 49730.48
Median num. threads observed: 48459.5
Min num. threads observed: 35891
Max num. threads observed: 63528
95% confidence: 48563.06 -- 50897.90


Untyped:
--------
Num. samples: 100
(Runtime)
Average running time: 10284.25
Median running time: 9425.0
Min running time: 2826
Max running time: 42300
95% confidence: 8974.73 -- 11593.77
(Thread Count)
Average num. threads observed: 60888.89
Median num. threads observed: 62324.5
Min num. threads observed: 45838
Max num. threads observed: 68685
95% confidence: 59590.98 -- 62186.80
