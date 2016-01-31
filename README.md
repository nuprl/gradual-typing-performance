gradual-typing-performance
===

The purpose of this repo is to share work among the team members that study
the performance hypothesis underlying gradual typing.


Overview
---
We have a few scripts in the project root, but most of the content is in a subfolder:
- `benchmarks`     : root folder for typed/untyped programs, the rolling suite of motivation
- `data`           : collected datasets
- `experimental`   : micro-benchmarks & unproven ideas
- `paper`          : writings on gradual typing performance
- `pre-benchmark`  : programs that are not yet ready to run
- `post-benchmark` : obsolete benchmarks
- `tools`          : scripts for understanding results & avoiding performance holes


Instructions on setting up an experiment
----------------------------------------

The steps that you need to take to set up an experiment are:

  * Install the `tools/benchmark-util` package.
  * Set up your project with a particular directory layout.
  * Modify typed modules to require and use `require/typed/check` instead of `require/typed`
  * Run `setup.rkt PROJECT-NAME` to generate all typed/untyped variations.

     ******************************************
     DO NOT MAKE CHANGES TO THE GENERATED FILES.
     ******************************************

  * Run `run.sh PROJECT-NAME` to benchmark all configurations

These steps are detailed below in separate sections.


Install package
---------------

Run `raco pkg install tools/benchmark-util`

Directory layout
----------------

The directory structure should look like the following for some project
called [project]:

  * [project]
    * [project]/base (put original files that don't get ported here)
    * [project]/both (files that are copied into each config)
    * [project]/typed (put typed ports of files here)
    * [project]/untyped (put original files here)
    * [project]/both (optional, put shared files that are copied into each variation)

The `typed` and `untyped` folders must contain files with the same
names. Additional libraries that shouldn't get ported (e.g., cannot add
types, has macros that don't need to be ported, etc.) should go in
`base`.

The `both` directory is like `base`, but for files that don't work with
the relative require path structure required for `base`. This directory
is optional.

See below for how to arrange the typed modules so that they can import
typed and untyped versions.


Modifications to typed modules
------------------------------

To work with the benchmark setup, typed modules in the `typed` folder need
to conditionally apply types to `require` statements because the linking
module may be typed or untyped depending on the variation.

First, in all typed modules that might end up requiring an untyped module
in the mixed program, add the following require:

````
  (require benchmark-util)
````

Second, if a module "a" requires "b" then the typed version of
"a" needs to change `(require b)` to something like
`(require/typed/checked b [f (-> t s)] [g (-> x y)])`.

The syntax of `require/typed/check` is the same as `require/typed`.

Finally, get the typed and untyped program to run via relative [to base]
paths.


Run setup.rkt
-------------

In the top directory, run the `setup.rkt` script on the project 

  `./setup.rkt [project]`

This will generate a `[project]/benchmark` folder containing the variations.
Each variation is in its own folder, plus a README file that specifies
where files come from.

If there is an existing `benchmark/` folder, it will be deleted.


Compile all variations
----------------------

In the top directory, call the driver script in compile-mode:

  `racket tools/run.rkt -c [project] [main.rkt]`

This will compile all variations to make sure that they build, but
will not run any of them.


Running the benchmark
---------------------
[project]
In the top directory, call the run script on the project:

  `sh run.sh [project]`


Presenting Results
------------------

The folder `tools/summarize` is a Racket package with tools for exploring and graphing a dataset.
To use, first install the package:

```
    raco pkg install tools/summarize
```

From the command-line, you now have a few options for processing `[dataset.rktd]`.

```
> raco gtp-explore [dataset.rktd]
# Open a REPL with query functions like `untyped-mean`
> raco gtp-sort [dataset.rktd]
# List all configurations from slowest to fastest
> raco gtp-lnm [dataset.rktd]
# Create an L-M/N plot for the data
```

Check `tools/summarize/README.md` for further detail.


Other notes
-----------

TODO:
- The various require/typed/* forms only work when both modules use #lang ...
   rather than (module Foo ...) because module->language-info is not always
   reliable, making it hard to determing when the other module is typed or not.
