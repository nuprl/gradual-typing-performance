Postmortem
==========

Analyzing the sources of contract overhead in our benchmarks

- `experiments` assorted tests and measurements
- `profile` output from the contract profiler, and analysis



Worst Variations
----------------

|------------+------------------+-------------+------------------------|
| Project    |       Worst Var. | Runtime(ms) | Overhead (vs. untyped) |
|------------+------------------+-------------+------------------------|
| gregor     |    0011001101001 |     3144.57 |                   4.72 |
| lnm        |           100100 |    49358.57 |                   1.14 |
| snake      |         11001011 |   120666.70 |                 121.51 |
| suffixtree |           011110 |   274769.03 |                 105.27 |
| synth      |       0010110111 |    22555.07 |                  85.90 |
| tetris     |        010100010 |    87645.97 |                 117.28 |
| quad       | 0100100000110101 |    11049.91 |                  56.43 |
|------------+------------------+-------------+------------------------|



Observations
------------

#### gregor
###### Worst
- Low contract overhead
- All contracts are predicates
- No predicates predominate (cost appears spread out)


###### Best
- Compile with adaptor (but the improvement is very small anyway)
- Top 3: only clock & offset-resolvers typed
  - not clear why from profile
  - clock: are parameters expensive? main calls current-clock, & also computes current times
    - else, clock seems to be MAKING things, passing in arguments
  - offsets: MAKE moment
- Top 4: Untyped
- Top 5-14: everything but moment (and possibly, main&clock)
  - moment's gotta be unpacking things
  - clock & moment are client & server
- Top 15: Typed


#### lnm
###### Worst
- Not very interesting, big contracts are on libraries
  - mean, from summary.rkt
  - draw, from lnm.rkt
- Some overhead on string?, between bitstring & summary

###### Best
- plot/summary typed. If either is, we're better than untyped


#### quad
###### Worst
- large contracts, all lists
- cost spread out, many contracts in log
- 2 higher order, kinda far down, functions are on Naturals/any

###### Best
- Wow, best off with the sample typed. This is confusing
  - yeah I need help


#### snake
###### Worst
- Lots of predicates (the most is posn?)
- Have 2 list contracts: snake-segs & cut-tail
  - Not sure to fix the struct accessor snake-segs

###### Best
- Typed seems the best, then untyped, then all-but-const.rkt
- Then its overhead


#### suffixtree
###### Worst
- the worst returns a vector
  (is there something wrong with contract-profile and lists?
   or is it because of what the lists contain -- struct? vs. char?)
- many predicate checks (snake-sized)
- follow/k is higher order, but appears kinda low

###### Best
- Fully typed, or untype main. Else overhead


#### synth
###### Worst
- much vector, much function on vector
  - same contract twice: array-unsafe-proc & unsafe-array-proc (re-bound)
- interesting that predicates don't appear much

###### Best
- Top: untyped. Top 2: typed.
- Top 40: seem "homogenous", but hard to tell at a glance


#### tetris
###### Worst
- all-on the block? predicate
- accessors & = also heavy
- nothing shows up on list

###### Best
- Top 30: very similar runtime
- All but (aux,const,main) should be typed, except maybe (tetras)


Lessons
-------
- The checks we see are nearly all predicates. These small things add up.
- Strong argument for soft contracts & type systems & failing that, JIT
- Some evidence that higher-order is slow; didn't appear much in the benchmarks
- But the profiler __doesn't show__ what triggers these checks


Experiments
-----------

#### tetris
- Reasoned that many `block?` checks were triggered by type `BSet = (Listof Block)`
- Added a "protector" between `bset.rkt` and `tetras.rkt`
- Changed the adaptor so that the `tetras` constructor unwrapped a wrapped bset
- Runtime improved from 36,000ms to 90ms; see `improved/tetris-1/`
- Similar results for `improved/tetris-5/`, where only `aux` and `block` are typed
  (Same change -- protect `bset`. Contract profile is similar but less drastic.)



On Adaptors
-----------

#### Why

- We used adaptors to help us run the lattice uniformly.
- Two typed modules that share untyped data NEED a single `require/typed` to share structs.
- Adaptor: make 1 module with the require/typed, require that instead
- Saves need to duplicate require/typed in new typed modules


#### Complaints

- Adaptors are not part of the original project (yes but you need some kind of revision)
- Adaptors add overhead in typed yes, from indirect requires (yes but small)


#### 1 vs. Many

Strategy: put all data in a single module, write 1 adaptor for it (once and for all)
- simple, makes all variations compatible
- no need to put functions in the adaptor
- avoids TR problems (untyped struct cannot inherit from a typed struct)

Problems:
- (small) introduces a bottleneck/contract-boundary between data-functions
- (large) by doing this, we changed the structure of some projects

###### gregor
- Moved `date`, `time`, `datetime`, `moment` structs to one file.
- Gregor already had a "core structs" file; we made a "gregor-structs" file
- TODO did this add much overhead?

###### quad
- Moved struct definitions from `ocm` and `penalty` into their own files
- Made typing & adaptor simpler, but added boundary
- TODO probably a small boundary, but I don't want to run

###### suffixtree
- Moved `label`, `tree`, and `node` structs to a new file.
- Created a LARGE bottleneck (but not the sole performance issue)
  - Down to 10x-usable

###### synth
- Moved `Mutable-Array` to a data file.
- Adds boundary to `array-stucts`.
- This added significant overhead : we are 10x-usable at L=1 now


### Without Adaptors

New experiment: how many variations compile without adaptors,
and are these the worst variations?

(HMMM this may be more a question of typed vs untyped data)


Summary:

- `gregor` : None compile. Too many structs, too much interaction.
- `lnm` : Almost all compile; the no-compiles are faster than untyped, but not the best.
  - (plot,summary) are typed in best-case
  - no-compiles have untyped summary & typed lnm, so avoid lnm contract overhead
- `snake` : Few bad ones compile.
  - Bad + compiles => these typed modules are isolated, and high overhead (try healing)
  - But generally bad=untyped-data, confirms that snake-segs is the expensive one (returning a big list)
- `suffixtree` : Very clustered
  - worst don't compile (100x -- 90x) = u:data,t:label
  - 2nd worst do compile (88x -- 70x) = t:data,u:label
  - below that, 44x
- `synth` : The worst compile. Typed data is a bad thing here, expensive to build
  - mixer builds a ton, via array-map. 26% of all contracts
  - transform builds a ton, via append. 12% of contracts
  - synth builds some, 8% of contracts (52 mil. checks total)
  - First no-compile is 60x (vs. 80x worst case).
- `tetris` : The worst compile without adaptor, but still many fail
  - untyped data is BAD, (t:block,u:bset) is BAD, tons of queries on data
  - i.e., the problem is indirect data access? the block wrapper kills us


Typed Overhead
--------------
Try running contract-profile on benchmarks with typed overhead

#### Gregor
- Boundary with TZ

#### Quad
- Hardly anything, all on vectors. ocm struct
- Possibly because it's a mutable field

#### mbta
- hm, nothing

#### zo
- 93% contracts, all predicates -- because of to-string "reflection" for sure
- small overhead on some accessors & in-library functions

