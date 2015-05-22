
The purpose of this repo is to share work among the team members that study
the performance hypothesis underlying gradual typing.

Auxillary folders:
- `mirco/` simulated micro gradual typing benchmarks
- `thoughts/` our first folder, for gathering ideas
- `tools/` scripts for creating, running, and visualizing the lattice
- `threads/` experiments debugging MBTA
- `paper/` contains the main performance evaluation paper
- `stop-2015` our STOP 2015 position paper

| Project name          | Responsible | Ported? | Benchmarked? | # Modules | Module structure |
| --------------------- | ----------- | ------- | ------------ | --------- | ---------------- |
| gregor                | Ben         | Yes     | No           | 13        | pyramidic        |
| HTDP script           | Ben         | Yes     | No           | 4         | triangle         |
| echo                  | Ben         | Yes     | No           | 4         | directed diamond |
| funkytown             | Ben         | Yes     | No           | 9         | vine-like        |
| kcfa                  | Ben         | Yes     | No           | 7         | line, or braid   |
| morse-code            | Ben         | Yes     | No           | 4         | vee              |
| sieve                 | Ben         | Yes     | No           | 2         | one chain        |
| suffixtree            | Ben         | Yes     | No           | 5         | line             |
| zo traversal          | Ben         | Yes     | No           | 5         | almost diamond   |
| mbta                  | Matthias    | Yes     | No           | 4         | one chain        |
| tetris                | Max         | Yes     | No           | 9         | diamond          |
| snake                 | Max         | Yes     | No           | 12        | diamond          |
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
