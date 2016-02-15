benchmark
===

Benchmark programs.

Each folder here should have at most 6 sub-folders:
- `untyped` : untyped benchmark code
- `typed` : typed benchmark code, matches the untyped code
- `base` : common helpers for the typed & untyped code. This may include frozen libraries.
- `both` : more common helpers*
- `benchmark` : contains all typed/untyped configs
- `extra` : additional, experimental folders. For instance modified versions of a benchmark.


#####  why `base/` and `both`?
Good question. TBH `both` came first but `base` evolved out of ignorance and convenience.
- `both/` is the "right" one, it copies code into each generated config
- `base/` is better for testing and avoids duplicate code (imagine making a copy of the `graph` library for each of `quad`'s configurations.
