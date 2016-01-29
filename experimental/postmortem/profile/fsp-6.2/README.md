Comparing FSP & contract-profile results

## Do the contract propertions match?

(These look good)

|------------+---------------+--------------|
| PROJECT    | MEAN (SE)     | FSP (SE)     |
|------------+---------------+--------------|
| gregor     | 83.29  (4.01) | 80.22 (2.73) |
| kcfa       | 91.38  (0.25) | 91.90 (0.16) |
| lnm        | 81.19  (0.73) | 81.23 (0.43) |
| mbta       | 39.02  (3.64) | 37.57 (3.53) |
| morse-code | 29.53  (6.81) | 31.15 (4.37) |
| quad       | 80.42  (0.96) | 79.97 (1.19) |
| sieve      | 91.93  (2.33) |              |
| snake      | 98.28  (0.21) | 97.59 (0.28) |
| suffixtree | 93.53  (0.18) | 93.50 (0.13) |
| synth      | 82.70  (1.22) | 82.97 (1.47) |
| tetris     | 95.67  (0.35) | 94.74 (0.71) |
| zordoz     | 79.40  (2.44) | 79.49 (1.43) |
|------------+---------------+--------------|

## Do the runtimes match?


|------------+------------------------+---------------------|
| PROJECT    | contract               | fsp                 |
|------------+------------------------+---------------------|
| gregor     | 1360.20 (395.14)       | 1091.00 (18.49)     |
| kcfa       | 480611.60 (17647.09)   | 430340.00 (9683.53) |
| lnm        | 54726.50 (3009.31)     | 51084.00 (388.25)   |
| mbta       | 1657.40 (122.54)       | 1583.70 (20.15)     |
| morse-code | 591.50 (142.15)        | 488.80 (10.55)      |
| quad       | 10865.00 (640.82)      | 9751.90 (92.84)     |
| sieve      | 1752402.60 (109340.95) |                     |
| snake      | 38377.80 (7159.69)     | 33465.10 (217.35)   |
| suffixtree | 302248.90 (4953.18)    | 284354.80 (2376.26) |
| synth      | 12441.30 (1823.63)     | 11233.30 (110.04)   |
| tetris     | 54983.30 (912.21)      | 54708.70 (2368.55)  |
| zordoz     |   762.00 (34.42)       |  1015.70 (12.74)    |
|------------+------------------------+---------------------|


#### kcfa
- store-join, store-update are worst in both
- only Ref? still here

#### lnm, mbta, morsecode, quad
- same before/after

#### snake
- snake-segs, world-snake, posn-x (how do we improve accessors?)

#### suffixtree
- label-datum, node-children, label-i, make-label (again, accessors)

#### synth
- Array, Array-unsafe-proc, unsafe-array-proc (redirected accessors)

#### tetris
- block=?
- block-x, block-y

#### zordoz
- predicates still expensive, lol (because of cond)
- zo-spec is the worst
- not interesting, even after fixing!
