extra
===

- `mbta+graph` : original, race-condition MBTA with the graph library included in the measurements
- `mbta-fixed+graph` : MBTA without the race condition and including the graph library
- `mbta-threaderror` : original MBTA, with the race condition

What race condition?
The main thread of the program infinitely loops, spawning threads.
So it's a race between the few productive threads, the main scheduler, and the idle child threads.
