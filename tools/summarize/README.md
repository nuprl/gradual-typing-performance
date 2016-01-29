summarize
===

Scripts for parsing and querying a gradual typing dataset.
Install via `raco pkg install ../summarize`


Usage
---

There are 3 modes of operation:

#### Explore a dataset

Explore the dataset `FILE.rktd` by running:

```
    raco gtp-explore FILE.rktd
```

This opens a REPL and loads the module `summary.rkt`.
From here, the next step is to create a summary object and start exploring:

```
> (define S (from-rktd FNAME)) ;; FNAME is bound with command-line arg
> (get-num-modules S)
```

See the API in `summary.rkt` for a full list of available functions.


#### Create an L-N/M plot

To render a dataset as an L-N/M figure, run:

```
    raco gtp-lnm FILE.rktd
```

There are many options you can pass to `gtp-lnm` to create different images.
Run `raco gtp-lnm --help` for a full list.


#### Sort configurations

For a sorted list of all configurations, run:

```
  raco gtp-sort FILE.rktd
```

By default, output is saved in `FILE-worst.rktd`.
The output will list all configurations sorted from slowest to fastest, along with the runtime & relative overhead of each.
