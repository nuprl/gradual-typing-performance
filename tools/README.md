Instructions
------------

To set up an experiment, create a folder <project> with the following
directory structure:

  * <project>
    ** <project>/base
    ** <project>/typed
    ** <project>/untyped

Then run the setup-benchmark.rkt script on the folder

  racket setup-benchmark.rkt <project>

This will generate a <project>/benchmark folder containing the variations.

Modify and use the driver script to run the variations.

Other notes
-----------

require-typed-utils.rkt exports several wrappers over TR's require/typed that
correctly handles when the current module is typed or untyped, and whether the
required module is typed/untyped.

TODO:
1. The various require/typed/* forms only work when both modules use #lang ...
   rather than (module Foo ...) because module->language-info is not always
   reliable, making it hard to determing when the other module is typed or not.
2. `create-benchmark-dirs` could be extended to handle special cases like
   needed for Acquire.
