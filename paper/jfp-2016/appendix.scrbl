#lang scribble/base
@require["common.rkt"]

MODULEGRAPHS
and use a graph structure to represent the interactions
 of its modules.
Nodes in the graphs represent modules in the program that our experiment
 varies as typed or untyped.
Edges represent static import statements.
For example, the leftmost node in each graph represents the program's main module,
 which imports from other modules but is never itself imported.
Finally, we color and thicken each edge in proportion to the run-time cost
 associated with the edge.
@todo{what are colors/what mean?}

---

TODO make barchart, or just dot+whisker chart

confidence intervals for untyped runtimes:
- *sieve  ((15785.96 . 16101.07) (16142.17 . 16424.09) (16216.82 . 16432.17))
- forth  ((1.79 . 2.13) (1.77 . 2.09) (2.21 . 2.56))
- *fsm  ((221.96 . 229.43) (185.98 . 191.61) (192.34 . 200.45))
- *fsmoo  ((480.87 . 493.52) (500.89 . 511.10) (465.86 . 481.93))
- *mbta  ((2066.16 . 2113.83) (2083.31 . 2110.81) (1790.20 . 1832.99))
- *morsecode  ((648.87 . 669.32) (608.15 . 622.18) (629.18 . 652.018))
- *zombie  ((8.04 . 8.53) (10.13 . 10.76) (8.06 . 8.51))
- dungeon  (0 0 0)
- **zordoz  ((829.28 . 836.71) (4114.01 . 4165.31) (2552.88 . 2573.31))
- **lnm  ((584.84 . 599.95) (1175.97 . 1191.62) (833.98 . 989.37))
- suffixtree  ((7918.23 . 8176.36) (7872.79 . 7996.47) (7961.95 . 8209.24))
- *kcfa  ((64633.42 . 66909.37) (130275.73 . 132329.99) (103122.77 . 105423.82))
- *snake  ((887.20 . 921.19) (949.59 . 961.34) (951.91 . 976.28))
- *take5  ((54.66 . 56.43) (182.77 . 185.62) (138.49 . 140.70))
- acquire  ((21122.03 . 21123.06) (21146.83 . 21147.74) (21134.67 . 21135.58))
- tetris  ((1243.37 . 1257.82) (1258.80 . 1271.59) (1236.43 . 1261.36))
- *synth  ((481.94 . 498.45) (458.78 . 469.61) (466.93 . 494.26))
- gregor  ((842.58 . 894.21) (815.64 . 845.75) (803.83 . 829.56))

