Lessons
=======

Gradual typing lessons and experiences: how to have the best possible gradual typing experience.

Note: the most important thing to remember about performance is that overheads come from typed/untyped boundaries.
Many of the recommendations here are about catching high-overhead boundaries, but these generalizations are not a great substitute for project-specific knowledge.
If you know the structure of your project, keep highly-connected modules together!

General Advice
--------------
- __Interfaces matter, but are not always stated.__
  Typed/Untyped boundaries need explicitly typed (i.e. contracted) boundaries.
  These are difficult to reconstruct when the developer used an `all-defined-out`.
  For the future, an `all-defined-out` is probably a hint that this boundary should
  not go through a protection barrier.
- __Compile often__.
  We found it useful to comment out the majority of a file and just verify that one
  function, or collection of functions, compiled.
- __Unit tests supercede types__.
  Adding types may subtly change the code's behavior, or require changes to the code
  itself. It is important to remember that the untyped code's behavior is the true
  spec for correctness, and this behavior is only captured in unit tests\* and comments.

  A small example of necessary code changes was due to Typed Racket's weak type for `partition`.
  Calling `partition` separates a list into two sub-lists, one with elements satisfying
  a given predicate and the other with all the rest.
  The types of both result lists should be refined through occurrence typing,
  but the list of negatives is currently not refined (Typed Racket issue [#138](https://github.com/racket/typed-racket/issues/138)).
  So we had to change the code to use two calls to `filter`, or else add a run-time assertion.
- __Type the Data First__.
  Try to assign, or at least identify, types for the core data structures in the project before anything else.
  For one, this tends to be easy to do because the data definition does not rely on other parts of the project.
  Second, even a vague type signature for the core data will help in solving the puzzle of which types belong elsewhere.

  Be sure to add types to functions tightly connected to the data as well.
  There should never be a boundary between a data structure and functions used often to manipulate it.
- __Use the typechecker to refine types.__
  If the type of a value is not clear from context, using a restrictive type like
  `(Void -> Void)` can give a helpful type error.
  This is especially true when the actual type is long, like `(HashTable String (Listof Boolean))`.
- __Favor assertions over strong types.__
  While porting, we generally did better using more permissive types like `Integer` or `(U #f Foo)` and using assertions to refine use-sites
  rather than trying to give a more specific type based on local observations.
  By "better", we mean we were less likely to encounter dead ends forcing us to change previous type annotations.
  Although, some dead ends were unavoidable, so:
- __Be wary of dead ends__.
  Especially with complex data structures, it's possible that a type works for
  a single file but not across files.
  A simple example is optimizing a vector library to only accept `Index` arguments.
  The file will typecheck, but its users may break because they passed `Integer` values
  (even though, these `Integer` values may be valid `Index` values!)
- __Some untyped designs will fail to typecheck__.
  Goes without saying. One memorable example was where a polymorphic function
  `(All (A) (-> (-> Void A) A))` was instantiated with a `(Values ...)` type.
  The untyped program ran, but the typechecker was overly conservative and rejected it.
- Lastly, asynchronous programming is hard.
  You never think it'll happen to you, but the issue we saw in MBTA taught us otherwise.

\* But beware _typed_ unit tests! (Typed Racket issue [#54](https://github.com/racket/typed-racket/issues/54))


Porting Strategies
------------------
We found that working bottom-up (fewest dependencies first) and top-down
(begin at arbitrary file) were both useful strategies.
Our recommendation is to work top-down, but we explain the benefits of each.


#### Least-dependencies first

Gradual typing went the smoothest when we could begin with files that had no
dependencies within the project, type-annotate those files, then move to
files that now only depended on typed modules.
Each file could then be run and tested immediately after adding types.

There are two issues with this approach.
First, it assumes a topological ordering on modules.
This is not always present, or easy to reconstruct.
In practice we could easily start with independent files and then move to their dependents, but the process was overall too manual.
Second is the issue of dead ends.
We occasionally needed to edit a previously-typed modules with different types to accomodate a later use-case.
It was difficult to predict where such dead-ends might occur when working bottom-up.


#### Most-dependencies first

Starting with the end-user module solved both the ordering and dead-end problems.
The `require` statements within each file identified its predecessors and there was no question of later users to make us change our locally-chosen type annotations.

Instead, the challenge was finding testable checkpoints to verify out annotations.
Each untyped dependency triggered a recursive search.
Luckily, there were two alternatives:

1. Type-annotate the required module (and address that module's dependencies too).
2. Identify the exact set of names imported from the untyped module and add a contract to each.
   After writing a contract boundary, the overall module would run (albeit slowly).

Step 2 worked well if the project was runnable and gradual typing did not add unacceptable overhead.
We could start by completely removing the import statement to get the set of names, and then refining overly-strict contracts to correct ones using run-time errors as a guide.
The feedback loop was definitely slow, but at least there was a feedback loop with the compiler to catch errors---just like in the bottom-up case---and also guide the search.

Realistically there will _always_ be a question of later users, but we found the feedback loop and better locality of this approach more tuned to the strengths of gradual typing.


Racket-Specific
---------------
- __Adaptor modules for untyped structs__.
  Any untyped data structure that is shared between typed modules needs to be
  adapted to account for Typed Racket's generative boundaries.
  We recommend using a specific adaptor module for this purpose.
  It is simpler and more straightforward than making one typed module the primary
  importer and having that typed module re-export the shared functions and data.

  This is NOT an excuse to put a typed/untyped boundary between a struct and closely-related functions.
  Those should all be typed (or untyped) as a single unit to avoid high overhead.
- __Consider opaque structs__.
  Typed Racket allows importing a struct opaquely, rather than exposing its internal fields.
  Opaque names lead to significantly faster contracts, but are also generative.
  This means that each opaque name technically requires an adaptor, and furthermore the operations on opaque data must be imported and exported explicitly.
- __Follow the Racket Style Guide__.
  The Racket style guide ([here](http://www.ccs.neu.edu/home/matthias/Style/style/Units_of_Code.html)) says that `provide` statements should be at the top of a file, and `require` statements similarly clustered.
  Additionally, each _module_ and each _function_ should have at least a purpose statement, and ideally tests.
  Unless the lead developer is the one adding types, it is much harder to recover type information without these small human artifacts.
