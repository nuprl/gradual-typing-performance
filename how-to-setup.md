Instructions on setting up an experiment
----------------------------------------

The steps that you need to take to set up an experiment are:

  * Install the "tools/benchmark-util" package.
  * Set up your project with a particular directory layout.
  * Modify typed modules to require and use `require/typed/check`
  * Run setup.rkt to generate all typed/untyped variations.

     ******************************************
     DO NOT MAKE CHANGES TO THE GENERATED FILES.
     ******************************************

  * Run benchmarks

These steps are detailed below in separate sections.

See the "example" folder in the top-level of this repo
for concrete examples for directory layout, how to modify typed
modules, and so on.

Install package
---------------

Run `raco pkg install tools/benchmark-util`

Directory layout
----------------

The directory structure should look like the following for some project
called [project]:

  * [project]
    * [project]/base (put original files that don't get ported here)
    * [project]/typed (put typed ports of files here)
    * [project]/untyped (put original files here)
    * [project]/both (optional, put shared files that are copied into each variation)

The `typed` and `untyped` folders should contain files with the same
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

In the top directory, call the run script in the tools folder:

  `racket tools/run.rkt -i [num-iterations] [project] [main.rkt]`

Arguments are:
- `[project]` the project folder's name (the script searches for `[project]/benchmark/variation*` folders)
- `[main.rkt]` the name of the project file to execute. Please call it `main.rkt`.
- `[num-iterations]` number of times to run each variation. (optional flag)


Other notes
-----------

TODO:
1. The various require/typed/* forms only work when both modules use #lang ...
   rather than (module Foo ...) because module->language-info is not always
   reliable, making it hard to determing when the other module is typed or not.
2. `create-benchmark-dirs` could be extended to handle special cases like
   needed for Acquire.
