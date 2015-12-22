summarize
===

Scripts for parsing and querying a gradual typing dataset.

Install via `raco pkg install ../summarize`


Usage
---

```
    raco summary FILE.rktd
```

Open a REPL session with data from `FILE.rktd` loaded into a `Summary` structure.
See `summary.rkt` to learn what to do with a `Summary` structure.

```
    raco render-lnm FILE.rktd
```

Make an L-N/M plot from the dataset `FILE.rktd`.
See `render-lnm.rkt` for details on how to build other graphs.


Examples
---

Coming
