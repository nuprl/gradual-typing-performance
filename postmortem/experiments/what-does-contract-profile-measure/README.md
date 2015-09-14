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


Full Results
------------

After ignoring `any->boolean` contracts globally:

|---+------------+----------------------+--------------------+-----------------------|
|   | Project    | Before    (std. dev) | a->b               | After      (std. dev) |
|---+------------+----------------------+--------------------+-----------------------|
|   | gregor     | 805           (10.4) | 142.9       (35.7) | 651            (12.8) |
|   | kcfa       | 397997.70 (11914.61) | 58369.60 (5064.58) | 330002.30  (11754.95) |
| * | lnm        | 50818.00   (1138.08) | 0                  | 53561.00    (1171.84) |
|   | mbta       | 1486.50      (17.69) | 0                  | 1481.40       (45.50) |
|   | morsecode  | 297.20        (4.79) | 0                  | 295.60         (2.58) |
|   | quad       | 9917.10      (85.86) | 4.20        (5.17) | 10165.00     (207.47) |
|   | sieve      | x                    | x                  | x                     |
|   | snake      | 21233.30    (610.66) | 8434.20   (306.29) | 10753.50     (146.19) |
|   | suffixtree | 491459.20  (2920.12) | 23093.65  (601.94) | 451899.90   (4189.54) |
| * | synth      | 7771.90     (196.39) | 0                  | 6093.70       (55.89) |
|   | tetris     | 32015.90    (384.77) | 10800.70  (229.42) | 18975.30     (237.51) |
|---+------------+----------------------+--------------------+-----------------------|

