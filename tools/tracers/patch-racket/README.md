patch-racket
============

#### How to Modify Racket to print typed contract logging information
1. Move `guts.rkt` to the "contract" folder.
  If `SRC` is the directory for your local Racket installation, then the file
  you need to replace is `SRC/racket/collects/racket/contract/private/guts.rkt`.
  Or just search using `find SRC -name "guts.rkt"`.
2. Move `require-contract.rkt` to the `typed/utils/` folder.
  That's probably `SRC/racket/share/pkgs/typed-racket-lib/typed-racket/utils/require-contract.rkt`.
3. Rebuild Racket, or just those two files and their dependencies.
4. Move `require-typed-utils.rkt` to your `benchmark-utils` package.
   This comments out the parts of `require/typed/check` that suppress contracts,
   and instead makes a contract for every identifier.

Note that, the total diff between these 2 files and their original counterparts is ~5 lines.


#### How to understand the logging information
Two kinds of messages are printed to `stdout` as the program runs.
- When `require/typed` creates a contract
- When a contract created by `require/typed` is used.

The first kind is prefixed by `[BG:CREATE]` and the second by `[BG:APPLY]`.
Otherwise, both messages have the same tab-separted format:
```
TYPE	UID	LOCATION	VALUE	SOURCE
```
- `TYPE` is either `[BG:CREATE]` or `[BG:APPLY]`
- `UID` is a unique identifier for this contract
- `LOCATION` is the file that created the contract
- `VALUE` is the name of the protected value
- `SOURCE` is the name of the file where the now-protected value originated
