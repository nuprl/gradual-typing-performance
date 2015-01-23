Implementations of various functions and macros that have been useful in running gradual typing performance experiments.


setup-benchmark.rkt exports the function `create-benchmark-dirs` which will setup all typed/untyped combinations in separate folders.


require-typed-utils.rkt exports several wrappers over TR's require/typed that correctly handles when the current module is typed or untyped, and whether the required module is typed/untyped.


TODO:
1. The various require/typed/* forms only work when both modules use #lang ... rather than (module Foo ...) because module->language-info is not always reliable, making it hard to determing when the other module is typed or not.
2. `create-benchmark-dirs` could be extended to handle special cases like needed for Acquire.
