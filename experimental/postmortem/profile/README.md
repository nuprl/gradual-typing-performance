profile
=======

contract-profile output for various benchmarks on their worst variations.


### Summary Table

| Project         | C/R (S.E.) | adaptor | higher-order | library | \tt{(T->any)} | \tt{(any->T)} | \tt{(any->bool)} |
| \tt{sieve}      | 92 (2.33)  |       0 |           46 |       0 |             0 |            54 |               31 |
| \tt{morse-code} | 29 (6.80)  |       0 |            0 |       0 |             0 |           100 |                0 |
| \tt{mbta}       | 39 (3.65)  |       0 |            0 |      65 |             0 |            65 |                0 |
| \tt{zo}         | 95 (0.10)  |       0 |            0 |      45 |             0 |            99 |               43 |
| \tt{suffixtree} | 94 (0.18)  |      98 |           <1 |       0 |             2 |            94 |               18 |
| \\tt{lnm}       | 81 (0.73)  |       0 |            9 |      99 |            91 |             0 |                0 |
| \tt{kcfa}       | 91 (0.26)  |     100 |            0 |       0 |             0 |            54 |               31 |
| \tt{snake}      | 98 (0.21)  |      93 |            0 |       0 |             1 |            78 |               49 |
| \tt{tetris}     | 96 (0.35)  |      89 |            0 |       0 |            11 |            89 |               44 |
| \tt{synth}      | 83 (1.22)  |       0 |           90 |       0 |            29 |            20 |                0 |
| \tt{gregor}     | 83 (4.01)  |      78 |            0 |       3 |             7 |            85 |               31 |
| \tt{quad}       | 80 (0.96)  |      <1 |           <1 |       0 |             3 |            <1 |               <1 |


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

