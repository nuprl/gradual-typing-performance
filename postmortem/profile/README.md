profile
=======

contract-profile output for various benchmarks on their worst variations.

### Summary Table

Proposed table for the paper.
Data taken using Racket v6.2

|------------+--------------+---+------------+-------+----------+-----------+-----------+--------|
| Project    | %C     (STE) |   | %any->bool |  %HOC | %library | %cod-only | %dom-only | %adapt |
|------------+--------------+---+------------+-------+----------+-----------+-----------+--------|
| gregor     | 83.32 (4.01) |   |      30.84 |     0 |     3.33 |     84.65 |      7.04 |  78.42 |
| kcfa       | 91.38 (0.26) |   |      31.02 |  0.02 |        0 |     31.03 |         0 | 100.00 |
| lnm        | 81.19 (0.73) |   |       9.36 | 99.14 |    99.52 |         0 |     90.63 |      0 |
| mbta       | 39.03 (3.65) |   |          0 |     0 |    64.95 |     64.95 |         0 |      0 |
| morsecode  | 29.60 (6.80) |   |          0 |     0 |        0 |    100.00 |         0 |      0 |
| quad       | 80.42 (0.96) |   |       0.08 |  0.53 |        0 |      0.51 |      3.23 |   0.31 |
| sieve      | 91.93 (2.33) |   |      31.08 | 46.27 |        0 |     31.08 |         0 |      0 |
| snake      | 98.28 (0.21) |   |      48.93 |     0 |        0 |     77.68 |      1.42 |  92.90 |
| suffixtree | 93.53 (0.18) |   |      17.55 |  0.23 |        0 |     93.97 |      2.09 |  97.91 |
| synth      | 82.70 (1.22) |   |          0 | 90.05 |        0 |     19.59 |     29.30 |      0 |
| tetris     | 95.67 (0.35) |   |      44.25 |     0 |        0 |     88.98 |     11.02 |  88.98 |
| zo         | 94.59 (0.10) |   |      43.19 |     0 |    44.84 |     99.01 |      0.00 |   0.00 |
|------------+--------------+---+------------+-------+----------+-----------+-----------+--------|


# gregor                                  # snake
  0. clock.rkt                              0. collide.rkt
  1. core-structs.rkt                       1. const.rkt
  2. date.rkt                               2. cut-tail.rkt
  3. datetime.rkt                           3. data.rkt
  4. difference.rkt                         4. handlers.rkt
  5. gregor-structs.rkt                     5. main.rkt
  6. hmsn.rkt                               6. motion-help.rkt
  7. main.rkt                               7. motion.rkt
  8. moment-base.rkt
  9. moment.rkt                           # suffixtree
  10. offset-resolvers.rkt                  0. data.rkt
  11. time.rkt                              1. label.rkt
  12. ymd.rkt                               2. lcs.rkt
                                            3. main.rkt
# lnm                                       4. structs.rkt
  0. bitstring.rkt                          5. ukkonen.rkt
  1. lnm-plot.rkt
  2. main.rkt                             # synth
  3. modulegraph.rkt                        0. array-broadcast.rkt
  4. spreadsheet.rkt                        1. array-struct.rkt
  5. summary.rkt                            2. array-transform.rkt
                                            3. array-utils.rkt
# quad                                      4. data.rkt
  0. exceptions.rkt                         5. drum.rkt
  1. hyphenate.rkt                          6. main.rkt
  2. main.rkt                               7. mixer.rkt
  3. measure.rkt                            8. sequencer.rkt
  4. ocm-struct.rkt                         9. synth.rkt
  5. ocm.rkt
  6. patterns-hashed.rkt                  # tetris
  7. penalty-struct.rkt                     0. aux.rkt
  8. quad-main.rkt                          1. block.rkt
  9. quads.rkt                              2. bset.rkt
  10. quick-sample.rkt                      3. consts.rkt
  11. render.rkt                            4. data.rkt
  12. sugar-list.rkt                        5. elim.rkt
  13. utils.rkt                             6. main.rkt
  14. world.rkt                             7. tetras.rkt
  15. wrap.rkt                              8. world.rkt

# morse-code                              # sieve
  0. levenshtein.rkt                        0. main.rkt
  1. main.rkt                               1. streams.rkt
  2. morse-code-strings.rkt
  3. morse-code-table.rkt                 # mbta
                                            0. main.rkt
                                            1. run-t.rkt
                                            2. t-graph.rkt
                                            3. t-view.rkt
# zordoz
  0. main.rkt
  1. zo-find.rkt
  2. zo-shell.rkt
  3. zo-string.rkt
  4. zo-transition.rkt

# kcfa
  0. ai.rkt
  1. benv.rkt
  2. denotable.rkt
  3. main.rkt
  4. structs.rkt
  5. time.rkt
  6. ui.rkt

