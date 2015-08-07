
The purpose of this repo is to share work among the team members that study
the performance hypothesis underlying gradual typing.

Auxillary folders:
- `micro/` simulated micro gradual typing benchmarks
- `thoughts/` our first folder, for gathering ideas
- `tools/` scripts for creating, running, and visualizing the lattice
- `threads/` experiments debugging MBTA
- `paper/` contains the main performance evaluation paper
- `stop-2015` our STOP 2015 position paper

| Project name          | Responsible | Ported? | Benchmarked? | # Modules | Module structure |
| --------------------- | ----------- | ------- | ------------ | --------- | ---------------- |
| gregor                | Ben         | Yes     | Yes          | 13        | pyramidic        |
| HTDP script           | Ben         | Yes     | Yes          | 4         | triangle         |
| echo                  | Ben         | Yes     | Yes          | 4         | directed diamond |
| funkytown             | Ben         | Yes     | Yes          | 9         | vine-like        |
| kcfa                  | Ben         | Yes     | Yes          | 7         | line, or braid   |
| morse-code            | Ben         | Yes     | Yes          | 4         | vee              |
| sieve                 | Ben         | Yes     | Yes          | 2         | one chain        |
| suffixtree            | Ben         | Yes     | Yes          | 6         | line             |
| quad                  | Ben         | Yes     | Yes          | 17        | tree             |
| zo traversal          | Ben         | Yes     | Yes          | 5         | almost diamond   |
| mbta                  | Matthias    | Yes     | Yes          | 4         | one chain        |
| tetris                | Max         | Yes     | Yes          | 9         | diamond          |
| snake                 | Max         | Yes     | Yes          | 12        | diamond          |
| protbuf (?)           |             | No      | No           |           |                  |
| [simulation][1] (?)   |             | No      | No           |           |                  |
| [ragg][2] (?)         |             | No      | No           |           |                  |
| [parser-tools][3] (?) |             | No      | No           |           |                  |
| [rmacs][4] (?)        |             | No      | No           |           |                  |

See "how-to-setup.md" in this repo for how to structure a project.

[1]: http://planet.racket-lang.org/display.ss?package=simulation.plt&owner=williams
[2]: https://github.com/jbclements/ragg/tree/master
[3]: https://github.com/racket/parser-tools
[4]: https://github.com/tonyg/rmacs
