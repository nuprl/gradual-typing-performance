###### Question:
what is the contract profiler measuring?

###### Proposal:
- [X] Test the overhead of (-> any/c boolean?) contracts
  - [X] Create an example program that spends significant time checking
  - [X] Edit "boolean?" to take a longer amount of time (expect a change)
  - [X] Edit the protected function to take longer (expect no change)
- [X] Extend results to (-> Any Boolean) contracts

###### Results:

As expected!

Slower contract => More time attributed to the (-> Any Boolean) contract.

Slower function => Less time (in proportion) attributed to the contract.

|----------+----------+--------+--------|
| flavor   | original | slow-c | slow-f |
|----------+----------+--------+--------|
| untyped  |    30.83 |  92.27 |  14.06 |
| typed    |    64.52 |  88.66 |  19.23 |
|----------+----------+--------+--------|
