forth3
===

Experiment: how to reduce slowdown?

- Attempt 1: move stack out of exec
-- no good, still very slow (idk how slow)

- Attempt 2: move env out of exec
-- down to 20,000 ms on the large test, 10ms on small

