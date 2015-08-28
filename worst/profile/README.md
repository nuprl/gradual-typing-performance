profile
=======

contract-profile output for various benchmarks on their worst variations.

### Summary Table

- `%C` is the proportion of runtime spent checking contracts,
  as reported by `contract-profile`
- `%PC` is the proportion of `%C` that was spent on `(-> Any Boolean)` contracts
- `%LC` is the proportion of `%C` spent on library contracts -- contracts on functions that lie outside the benchmark files, for example in `compiler/zo-lib`.
- `%HOC` is the proportion of `%C` spent on higher-order contracts -- any contract with an `->` in a negative position. (All reported contracts have the form `(-> ...)`. This column documents the `(-> ... (-> ...) ...)` forms.

Note that the `%PC` and `%LC` columns may overlap.
The `%LC` and `%HOC` also overlap.


|------------+------------------+-------------+------------------------+-------+-------+-------+-------|
| Project    |       Worst Var. | Runtime(ms) | Overhead (vs. untyped) |   % C |  % PC |   %LC |  %HOC |
|------------+------------------+-------------+------------------------+-------+-------+-------+-------|
| gregor     |    0011001101001 |     3144.57 |                   4.72 | 68.09 | 19.34 |  6.29 |     0 |
| kcfa       |          1001001 |  1017144.57 |                  22.67 | 86.89 | 22.19 |     0 |     0 |
| lnm        |           100100 |    49358.57 |                   1.14 | 81.97 |     0 | 99.52 |  9.17 |
| mbta       |             1110 |      149.84 |                   2.80 |     0 |     0 |     0 |     0 |
| morsecode  |             0101 |   133414.93 |                   1.95 | 22.77 |     0 |     0 |     0 |
| quad       | 0100100000110101 |    11049.91 |                  56.43 | 80.09 |     0 |     0 |  0.51 |
| sieve      |               10 |  1313641.77 |                 114.47 | 95.15 | 59.16 |     0 | 28.38 |
| snake      |         11001011 |   120666.70 |                 121.51 | 96.65 | 42.74 |     0 |     0 |
| suffixtree |           011110 |   274769.03 |                 105.27 | 92.18 | 11.73 |     0 |  0.21 |
| synth      |       0010110111 |    22555.07 |                  85.90 | 81.95 |     0 |     0 | 88.45 |
| tetris     |        010100010 |    87645.97 |                 117.28 | 93.54 | 42.58 |     0 |     0 |
| zo         |            11101 |     2200.00 |                   4.33 | 94.80 | 43.67 | 45.26 | 54.74 |
|------------+------------------+-------------+------------------------+-------+-------+-------+-------|

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

