spawn-1000-threads
==================

Small test to see if typed racket has a smaller cost of spawning threads.
Hypothesis: threads run faster in typed racket (because MBTA was faster with
types, and the majority of mbta's runtime was spent on threads).

Results
-------
Average untyped runtime (100 runs): 48ms
Average typed   runtime (100 runs): 480ms

Yes, a factor of 10.
I don't think typed racket is helping.

Description
-----------
Create a custodian.
In a loop, create lots of threads.
Give each thread the function `(lambda () (void))`.
Do not name threads.
Outside the loop, use the custodian to clean up.

Next Steps
----------
MBTA actually used its threads.
They communicated over input/output ports.
Try testing the latency of output ports, or the latency of thread communication in general.
It's definitely ok to serialize things because that's what mbta did.
