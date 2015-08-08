tracers
=======

Answering the question of HOW values flow across different program components at runtime.
(Remember Jan's highway analogy)

Contains files to modify a Racket installation to produce log messages for `require/typed` contracts,
a few scripts to process this data,
and results for our gradual typing projects.


Summary
-------
High-level summary of each directory in this folder:

- `data/` the RESULTS for gradual typing projects
- `examples/` a few small projects, sanity checks to be sure the logging happens correctly
- `scripts/` code to process generated logs
- `patch-racket/` two files used to add log information when running racket

See the `README.md` file in each directory for more details.




A case for ADTs
===============
Contracts on data are overwhelmingly expensive.
Here's evidence.

Exhibit A
---------

Contract counts for worst-case projects


| project    | data contracts | % total | data checks | % total |
|------------+----------------+---------+-------------+---------|
| echo       |              - |       - | -           |       - |
| sieve      |              - |       - | -           |       - |
| morsecode  |              - |       - | -           |       - |
| mbta       |              4 |       3 | 330         |      73 |
| zordoz     |            211 |       1 | 583,892     |     100 |
| suffixtree |             20 |      30 | 494,695,557 |      97 |
| lnm        |             18 |       8 | 8,013       |      32 |
| kcfa       |             48 |     100 | 877,928,995 |     100 |
| snake      |             13 |      40 | 136,566,409 |      93 |
| tetris     |             17 |      32 | 169,689,972 |      99 |
| synth      |             12 |      19 | 24,681,549  |      47 |
| gregor     |             43 |      31 | 2,880,916   |      78 |
| quad       |            169 |      54 | 81,744      |      45 |



Exhibit B
---------

Average runtime of variations in TWO GROUPS for each file.
First group has the file never in a boundary.
Second group has the file always in a boundary.
Also shown: the percent difference between the always and never columns.


#### Suffixtree
FILE     NEVER      ALWAYS      PCT.DIFF
data     4811.99    169186.01  3415.92
label    114455.20  165263.70  44.39
lcs      146902.72  170922.55  16.35
structs  140960.92  164896.54  16.98
ukkonen  156557.84  161267.43  3.01

#### Snake
FILE          NEVER     ALWAYS    PCT.DIFF
collide       19725.36  21245.20  7.71
const         20342.82  20505.63  0.80
cut-tail      19162.37  21808.19  13.81
data          511.20    20642.56  3938.06
handlers      19633.57  21336.99  8.68
motion-help   19881.17  21089.39  6.08
motion        19068.29  20957.61  9.91

#### Tetris
FILE    NEVER     ALWAYS    PCT.DIFF
aux     25109.25  24721.02  -1.55
block   14383.35  28296.32  96.73
bset    26441.95  24709.82  -6.55
consts  25339.09  24783.35  -2.19
data    736.01    25007.70  3297.75
elim    25164.79  24471.37  -2.76
tetras  24620.34  24883.99  1.07
world   25172.27  24463.89  -2.81

#### Gregor
FILE              NEVER    ALWAYS   PCT.DIFF
clock             1606.94  1609.78  0.18
core-structs      826.99   1611.42  94.85
date              1428.74  1634.02  14.37
datetime          1413.83  1611.45  13.98
difference        1594.83  1621.89  1.70
gregor-structs    987.69   1609.57  62.96
hmsn              1419.99  1614.44  13.69
moment-base       1588.02  1615.14  1.71
moment            1585.92  1615.84  1.89
offset-resolvers  1608.37  1608.35  -0.00
time              1540.03  1631.14  5.92
ymd               1561.42  1624.01  4.01

#### Funkytown
FILE             NEVER     ALWAYS    PCT.DIFF
array-broadcast  10248.54  10453.20  2.00
array-struct     6035.88   10471.34  73.48
array-transform  10384.22  10407.97  0.23
array-utils      7702.42   10489.12  36.18
data             4391.55   10802.73  145.99
drum             10398.02  10406.04  0.08
mixer            10319.23  10429.64  1.07
sequencer        10314.13  10489.94  1.70
synth            10205.10  10430.17  2.21

#### Kcfa
FILE      NEVER      ALWAYS     PCT.DIFF
ai        416356.72  400062.50  -3.91
benv      44916.88   432429.13  862.73
denotable 44844.14   529331.43  1080.38
structs   44976.90   413975.21  820.42
time      44902.26   460110.66  924.69
ui        410176.28  406242.94  -0.96

#### Quad
FILE            NEVER    ALWAYS   PCT.DIFF
exceptions      6167.22  6168.76  0.02
hyphenate       6167.04  6168.95  0.03
measure         5581.25  6251.81  12.01
ocm-struct      5895.76  6258.74  6.16
ocm             6150.04  6185.95  0.58
patterns-hashed 6165.70  6170.29  0.07
penalty         6160.86  6175.13  0.23
quad-main       6165.01  6170.98  0.10
quads           1836.15  6307.73  243.53
quick-sample    6165.79  6170.20  0.07
render          6158.56  6177.43  0.31
sugar           5996.34  6225.21  3.82
utils           5561.17  6254.68  12.47
world           5547.96  6187.99  11.54
wrap            6028.60  6307.38  4.62
