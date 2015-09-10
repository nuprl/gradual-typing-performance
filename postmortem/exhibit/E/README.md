Total runtime before and after ignoring certain contracts.
(It's TOTAL, not contract runtime)

|------------+------------------+--------+---------+--------|
| Project    |       Worst Var. | Before |    a->b |  After |
|------------+------------------+--------+---------+--------|
| gregor     |    0011001101001 |    803 |     150 |    850 | *
| kcfa       |          1001001 | 384860 |   54232 | 318097 |
| lnm        |           100100 |      x |       x |        |
| mbta       |             1110 |      x |       x |        |
| morsecode  |             0101 |      x |       x |        |
| quad       | 0100100000110101 |   9877 |       x |        |
| sieve      |               10 |      x |       x |        |
| snake      |         11001011 |  21057 |  8481.5 |  13110 |
| suffixtree |           011110 | 489683 | 22781.5 | 458620 |
| synth      |       0010110111 |   7730 |       0 |   6256 | *
| tetris     |        010100010 |  33056 | 11223.5 |  17427 |
|------------+------------------+--------+---------+--------|


TODO average of 10 runs

