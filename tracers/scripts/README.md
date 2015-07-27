scripts
=======

Scripts to create and read traced contracts.

The main one is `trace-run.rkt`. Usage:

```
racket trace-run.rkt DIR
```

where `DIR` is a project directory, like `suffixtree/` or `quad/`.

- Runs the fully-typed version of the project
- Collects contract usage information
- Displays aggregate stats on run-time contract use

Add the `-o FILENAME` option to fill `FILENAME` with data for every contracted identifier.
