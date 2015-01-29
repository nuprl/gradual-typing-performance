Instructions on setting up an experiment
----------------------------------------

To set up an experiment, create a folder <project> with the following
directory structure:

  * <project>
    ** <project>/base (put original files that don't get ported here)
    ** <project>/typed (put typed ports of files here)
    ** <project>/untyped (put original files here)

The `typed` and `untyped` folders should contain files with the same
names. Additional libraries that shouldn't get ported (e.g., cannot add
types, has macros that don't need to be ported, etc.) should go in
`base`.

Then run the `setup.rkt` script on the project:

  ./setup.rkt <project>

This will generate a <project>/benchmark folder containing the variations.
If there is an existing folder, it will be deleted. So do not make changes
to the generated files.

Instructions for porting untyped modules
----------------------------------------

To work with the benchmark setup, typed modules in the `typed` folder need
to conditionally apply types to `require` statements because the linking
module may be typed or untyped depending on the variation.

Running `./setup.rkt` will automatically install the "benchmark-util"
package, which provides a `require/typed/check` macro for this purpose.

The changes you need to make are to add a single require statement to
typed modules that require other modules:

````
  (require benchmark-util)
````

Then replace uses of `require` with `require/typed/check`. The syntax of
`require/typed/check` is the same as `require/typed`.

Examples
--------

The "example" folder in the top-level of this repo contains an example
of the required directory structure and uses of `require/typed/check`.

Other notes
-----------

TODO:
1. The various require/typed/* forms only work when both modules use #lang ...
   rather than (module Foo ...) because module->language-info is not always
   reliable, making it hard to determing when the other module is typed or not.
2. `create-benchmark-dirs` could be extended to handle special cases like
   needed for Acquire.
