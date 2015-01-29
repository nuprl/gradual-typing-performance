Instructions on setting up an experiment
----------------------------------------

The steps that you need to take to set up an experiment are:

  * Install the "tools/benchmark-utils" package.
  * Set up your project with a particular directory layout.
  * Modify typed modules to require and use `require/typed/check`
  * Run setup.rkt to get all typed/untyped variations.
  * Run benchmarks

These steps are detailed below in separate sections.

See the "example" folder in the top-level of this repo
for concrete examples for directory layout, how to modify typed
modules, and so on.

Install package
---------------

Run `raco pkg install tools/benchmark-utils`

Directory layout
----------------

The directory structure should look like the following for some project
called [project]:

  * [project]
    * [project]/base (put original files that don't get ported here)
    * [project]/typed (put typed ports of files here)
    * [project]/untyped (put original files here)

The `typed` and `untyped` folders should contain files with the same
names. Additional libraries that shouldn't get ported (e.g., cannot add
types, has macros that don't need to be ported, etc.) should go in
`base`.

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

Then, if a module "a" requires "b" then the typed version of
"a" needs to change `(require b)` to something like
`(require/typed/checked b [f (-> t s)] [g (-> x y)])`.

The syntax of `require/typed/check` is the same as `require/typed`.

Run setup.rkt
-------------

Run the `setup.rkt` script on the project to generate the variations in its
own folder that can be run by the driver script (described below):

  ./setup.rkt [project]

This will generate a [project]/benchmark folder containing the variations.
If there is an existing folder, it will be deleted. So do not make changes
to the generated files.

Running the benchmark
---------------------

This section will be filled in later.

Other notes
-----------

TODO:
1. The various require/typed/* forms only work when both modules use #lang ...
   rather than (module Foo ...) because module->language-info is not always
   reliable, making it hard to determing when the other module is typed or not.
2. `create-benchmark-dirs` could be extended to handle special cases like
   needed for Acquire.
