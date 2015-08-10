#lang racket/base

;; Run a Racket file while tracking contract-count output.
;; - Print summary results to console
;; - (Optionally) given a `-o FILE.rktd` filename, save detailed result to file

(provide
 filename=?/adapted
 ;; (-> String String Boolean)
 ;; True if two filenames are equal,
 ;; or equal after removing the string '-adapted' from each.
 ;; (In retrospect, this was not a good idea because the `file -> file-adapted`
 ;;  boundaries are very important)
)

;; -----------------------------------------------------------------------------

(require
  glob
  (only-in racket/list first second third fourth fifth last)
  (only-in racket/match match-define)
  (only-in racket/system system process)
  (only-in racket/string string-join string-split string-replace)
  (only-in racket/file copy-directory/files)
)

;; =============================================================================
;; -- constants

;; Higher value = Print more debug information
(define DEBUG 1)

;; -----------------------------------------------------------------------------
;; -- unused functions

;; Create benchmark directory.
;; This should really be done lazily.
;; (: create-configurations (-> Path-String Void))
(define (create-configurations parent-dir)
  (raise-user-error 'trace-run "Benchmark directory not found. Create it, then try again."))

;; Run a single benchmark configuration
;; (: run-one (-> Path-String Void))
(define (run-one dirname)
  (parameterize ([current-directory dirname])
    (system "racket main.rkt > stdout.log" #:set-pwd? 'unix)))

;; Run all benchmark configurations under the parent directory
;; (: run-all (-> Path-String Void))
(define (run-all parent-dir)
  (define benchmark-dir (format "~a/benchmark" parent-dir))
  (unless (directory-exists? benchmark-dir)
    (create-configurations parent-dir))
  (for ([dirname (in-glob (format "~a/variation*" benchmark-dir))])
    (printf "Running '~a'\n" dirname)
    (run-one dirname)))

;; -----------------------------------------------------------------------------
;; -- util

(define-syntax-rule (arg-error msg arg* ...)
  (raise-user-error 'trace-run:commandline (format msg arg* ...)))

(define-syntax-rule (debug N msg arg* ...)
  (when (<= N DEBUG) (displayln (format msg arg* ...))))

(define-syntax-rule (precondition-error msg arg* ...)
  (raise-user-error 'trace-run:precondition (format msg arg* ...)))

(define-syntax-rule (warning msg arg* ...)
  (displayln (string-append "WARNING: " (format msg arg* ...))))

;; If `dirname` does not exist, create it
;; (: ensure-directory (->* [Path-String] [(Sequenceof Path-String)] Void))
(define (ensure-directory dirname #:fill-with [fill-with '()])
  (unless (directory-exists? dirname)
    (make-directory dirname))
  (for ([path fill-with])
    (define dest (format "~a/~a" dirname (path-last path)))
    (if (directory-exists? path)
        ;; The docs don't show how to copy+clobber a directory
        (system (format "cp -r ~a ~a" path dest))
        (copy-file path dest #t))))

;; Not implemented.
;; THE IDEA: check that the user's racket installation matches the files in the `patch-racket` directory
;; THE PROBLEM: not sure how to find the racket installation
;; (: ensure-hacked-racket (-> Void))
(define (ensure-hacked-racket)
  (void))

;; Assert that `dirname` has all the characteristics of a GT benchmark
;; - has untyped folder
;; - has typed folder
;; - same filenames in the typed & untyped folders
;; (: ensure-preconditions (-> Path-String Void))
(define (ensure-preconditions dirname)
  ;; Check that typed/ and untyped/ directories exist
  (define ty-dir (format "~a/typed" dirname))
  (define un-dir (format "~a/untyped" dirname))
  (unless (directory-exists? ty-dir) (precondition-error "Expected directory '~a' does not exist" ty-dir))
  (unless (directory-exists? un-dir) (precondition-error "Expected directory '~a' does not exist" un-dir))
  ;; Check same filenames
  (define ty-file* (glob (format "~a/*.rkt" ty-dir)))
  (define un-file* (glob (format "~a/*.rkt" un-dir)))
  (unless (= (length ty-file*) (length un-file*))
    (precondition-error "Expected typed/ and untyped/ directories to contain the same number of files, but typed has ~a files and untyped has ~a files." (length ty-file*) (length un-file*)))
  (for ([ty-rkt (in-list ty-file*)]
        [un-rkt (in-list un-file*)])
    (unless (equal? (path-last ty-rkt) (path-last un-rkt))
      (precondition-error "Expected typed and untyped directories to have the same filenames, but found mismatch '~a' != '~a'" ty-rkt un-rkt)))
  (void))

;; Return the names of the experiment modules in the file
;; (: get-module-names (-> Path-String (Listof String))
(define (get-module-names dirname)
  (for/list ([full-path (in-glob (format "~a/typed/*.rkt" dirname))])
    (path-last full-path)))

;; Count the number of modules in the project rooted at `dirname`
;; (: get-num-modules (-> Path-String Natural))
(define (get-num-modules dirname)
  (length (get-module-names dirname)))

;; Get the last part (the filename + extension) from a path.
;; (: path-last (-> Path String))
(define (path-last p)
  (path->string (last (explode-path p))))

;; Update the directory `unclean-dir` with files from `parent-dir`.
;; If optional argument #:from is omitted, `parent-dir` is inferred
;;  as the first part of the path in `unclean-dir`.
;; (: refresh-dir (->* [Path-String] [#:from Path-String] Void))
(define (refresh-dir unclean-dir #:from [parent-dir-opt #f])
  (ensure-directory unclean-dir)
  (define parent-dir (or parent-dir-opt (car (string-split unclean-dir "/"))))
  ;; Copy source files
  (for ([ty-rkt (in-glob (format "~a/typed/*.rkt" parent-dir))])
    (define rkt (path-last ty-rkt))
    (copy-file ty-rkt (format "~a/~a" unclean-dir rkt) #t))
  ;; Copy "both" files, if the directory exists
  (define both-dir (format "~a/both" parent-dir))
  (when (directory-exists? both-dir)
    (for ([both-rkt (in-glob (format "~a/*.rkt" both-dir))])
      (define rkt (path-last both-rkt))
      (copy-file both-rkt (format "~a/~a" unclean-dir rkt) #t)))
  (void))

;; True if the second argument is a prefix of the first
;; (: string-startswith? (-> String String String))
(define (string-startswith? str prefix)
  (for/and ([c1 (in-string str)]
            [c2 (in-string prefix)])
    (char=? c1 c2)))

;; True if the second argument is a suffix of the first
;; (: string-endswith? (-> String String String))
(define (string-endswith? str suffix)
  ;; Skip all but the last `suffix` characters of `str`
  (define offset (- (string-length str) (string-length suffix)))
  (for/and ([c1 (in-string str offset)]
            [c2 (in-string suffix)])
    (char=? c1 c2)))

;; -----------------------------------------------------------------------------
;; -- parsing results (logs -> contract information)

;; Data representing a line of output about contracts.
;; Log messages are always tab-separated (in Ben's instrumented Racket)
;;   TAG    ID    TO-FILE    VALUE    FROM-FILE
(struct contract-log (
  tag       ;; (U 'create 'apply)
  uid       ;; Natural : A unique identifier for each created contract
  to-file   ;; String  : Name of file that imported the value and created the contract
  value     ;; String  : Name of the value wrapped in a contract
  from-file ;; String  : Name of the file that provided the value
) #:transparent)
;; (define-type ContractLog contract-log)

;; Convert a line of log information to a struct
;; (: string->contract-log (-> String ContractLog))
(define (string->contract-log tag line)
  (define tag+id+to+val+from (string-split line))
  (contract-log
   tag
   (second tag+id+to+val+from)
   (third  tag+id+to+val+from)
   (fourth tag+id+to+val+from)
   (fifth  tag+id+to+val+from)))

;; If `line` is a message logging a new contract, return a `contract-log` struct
;; (: bg-create? (-> String (U ContractLog #f)))
(define (bg-create? line)
  (cond
   [(string-startswith? line "[BG:CREATE]")
    (string->contract-log 'create line)]
   [else #f]))

;; If `line` is a message logging a boundary contract application,
;; return a `contract-log` struct
;; (: bg-apply? (-> String (U ContractLog #f)))
(define (bg-apply? line)
  (cond
   [(string-startswith? line "[BG:APPLY]")
    (string->contract-log 'create line)]
   [else #f]))

;; Add a new contract, represented by `clog`, to the map `cmap`.
;; (: add-contract (-> ContractUsageMap (-> ContractLog Void)))
(define ((add-contract cmap) clog)
  (match-define (contract-log tag uid to-file val from-file) clog)
  ;; Get the hashtable for contracts out of `from-file`, else create it.
  (define from-hash (hash-ref cmap from-file (lambda () #f)))
  (cond
   [(not from-hash)
    (hash-set! cmap from-file (make-hash (list (cons to-file (make-hash (list (cons val 0)))))))]
   [else
    ;; Get the hashtable for contracts in to `to-file`, else create a new one
    (define to-hash (hash-ref from-hash to-file (lambda () #f)))
    (cond
     [(not to-hash)
      (hash-set! from-hash to-file (make-hash (list (cons val 0))))]
     [else
      ;; Get the hashtable for contracts on the value `val`
      ;; (This should always fail)
      (define val-count (hash-ref to-hash val (lambda () #f)))
      (cond
       [(not val-count)
        (hash-set! to-hash val 0)]
       [else
        (warning "Duplicate contract on value '~a', from '~a' to '~a'. (new id '~a')" val from-file to-file uid)])])]))

;; Increment the usage count for the contract `clog` in the map `cmap`.
;; (: add-contract (-> ContractUsageMap (-> ContractLog ContractUsageMap)))
(define ((update-contract cmap) clog)
  (define (missing-key key)
    (error 'trace-run:update-contract "Missing key '~a', cannot update information for contract '~a'" key clog))
  (match-define (contract-log tag uid to-file val from-file) clog)
  (define from-hash (hash-ref cmap from-file (lambda () #f)))
  (cond
   [(not from-hash)
    (missing-key from-file)]
   [else
    ;; Get the hashtable for contracts in to `to-file`, else create a new one
    (define to-hash (hash-ref from-hash to-file (lambda () #f)))
    (cond
     [(not to-hash)
      (missing-key to-file)]
     [else
      (define val-count (hash-ref to-hash val (lambda () #f)))
      (cond
       [(not val-count)
        (missing-key val)]
       [else
        (hash-set! to-hash val (add1 val-count))])])]))

;; -----------------------------------------------------------------------------
;; -- show (display parsed logging information -- show me what the contracts did)

(struct boundary (
  from-file ;; String  : where contracts came from
  to-file   ;; String  : where contracts went to
  val       ;; String  : the identifier covered with a contract
  checks    ;; Natural : the number of times the identifier was used
) #:prefab)
;; (define-type Boundary boundary)

;; True if `b1` is "more expensive" than `b2`. i.e. is called more
;; (: boundary<? (-> Boundary Boundary Boolean))
(define (boundary>? b1 b2)
  (> (boundary-checks b1) (boundary-checks b2)))

(define (filename=?/adapted f1 f2)
  (or (string=? f1 f2)
      (string=? (string-replace f1 "-adapted" "")
                (string-replace f2 "-adapted" ""))))

;; Return a list of all contracts, represented as `boundary` structs
;; The list is partially sorted; for each from/to pair, the most expensive boundaries come first.
;; (: all-boundaries (-> ContractUsageMap (Listof Boundary)))
(define (all-boundaries from->to #:valid-filenames [valid? #f])
  (for*/list ([(from to->id) (in-hash from->to)]
              [(to id->nat) (in-hash to->id)]
              #:when (or (not valid?)
                         (and (valid? from) (valid? to))))
    (sort
     (for/list ([(val count) (in-hash id->nat)]) (boundary from to val count))
     boundary>?)))
;; Informal documentation for output .rktd files.
;; Belongs at the top of these printed files.
(define DATA-FORMAT
  (string-join '(";; Data is a list of lists of boundary structures"
                 ";; There is one inner list for each boundary in the program"
                 ";; The boundary structures have 4 fields"
                 ";; - from-file : String"
                 ";; - to-file  : String"
                 ";; - val : String"
                 ";; - checks : Natural")
               "\n"))

;; Fold over a `ContractUsageMap`.
;; (: fold-cmap (All (A) (->* [(-> A Natural A) A ContractUsageMap] [#:from (U #f String) #:to (U #f String)] A)))
(define (fold-cmap f init from->to #:from [only-from #f] #:to [only-to #f])
  (for*/fold ([acc init])
             ([to->id (or (and only-from (cond [(hash-ref from->to only-from (lambda () #f))
                                               => (lambda (x) (list x))]
                                              [else '()]))
                         (in-hash-values from->to))]
             [id->nat (or (and only-to (cond [(hash-ref to->id only-to (lambda () #f))
                                              => (lambda (x) (list x))]
                                             [else '()]))
                          (in-hash-values to->id))]
             [num-checks (in-hash-values id->nat)])
    (f acc num-checks)))

;; Count the total number of contracts represented in the map
;; (: count-contracts (->* [ContractUsageMap] [#:from (U #f String) #:to (U #f String)] Natural))
(define (count-contracts from->to #:from [only-from #f] #:to [only-to #f])
  (fold-cmap (lambda (acc checks) (add1 acc))
             0
             from->to
             #:from only-from
             #:to only-to))

;; Count the total number of contract checks / applications in the map
;; (: count-checks (->* [ContractUsageMap] [#:from (U #f String) #:to (U #f String)] Natural))
(define (count-checks from->to #:from [only-from #f] #:to [only-to #f])
  (fold-cmap (lambda (acc checks) (+ acc checks))
             0
             from->to
             #:from only-from
             #:to only-to))

;; Return a list of the worst contracts, one for each pair of files.
;; Sort results in order of "most checks" to "fewest checks"
;; (: filter-worst-boundaries (-> ContractUsageMap (Listof Boundary)))
(define (filter-worst-boundaries from->to #:valid-filename? [valid? #f])
  (define unsorted-worst
    ;; For each from/to pair, pick the most expensive value
    (for*/list ([(from to->id) (in-hash from->to)]
                [(to id->nat)  (in-hash to->id)]
                #:when (or (not valid?) ;; No filter = accept everything
                           (and (valid? from) (valid? to))))
      ;; Make a boundary struct with each
      (define-values (best-val best-count)
        (for/fold ([bval #f] [bcnt #f])
                  ([(val count) (in-hash id->nat)])
          (if (or (not (and bval bcnt))
                  (< bcnt count))
              (values val count)
              (values bval bcnt))))
      (boundary from to best-val best-count)))
  (sort unsorted-worst boundary>?))

;; Convert a boundary struct to a pretty string
;; (: format-boundary (-> Boundary String))
(define (format-boundary bnd)
  (match-define (boundary from to val checks) bnd)
  (format "[~a => ~a] value '~a' checked ~a times" from to val checks))

;; -----------------------------------------------------------------------------
;; -- main

;; Database of contracts created & used during program execution
;;
;; (define-type Filename Symbol)
;; (define-type Identifier Symbol)
;; (define-type ContractUsageMap
;;   (HashTable Filename ;; FROM
;;              (HashTable Filename ;; TO
;;                         (HashTable Identifier ;; VALUE
;;                                    Natural))))

;; Run the fully-typed version of the project,
;; collect run-time contract information (num created & used)
;; return a map of results
;; (: collect-contract (-> Path-String ContractUsageMap))
(define (collect-contract parent-dir)
  (debug 1 "Checking preconditions for '~a'..." parent-dir)
  ;; Prepare the fully-typed configuration
  (define num-modules (get-num-modules parent-dir))
  (define typed-benchmark-dir
    (format "~a/benchmark/variation~a" parent-dir (make-string num-modules #\1)))
  (refresh-dir typed-benchmark-dir #:from parent-dir)
  ;; Enter the fully-typed directory, run `main.rkt`
  (define contract-log (make-hash '()))
  (define add-contract/log    (add-contract    contract-log))
  (define update-contract/log (update-contract contract-log))
  (parameterize ([current-directory typed-benchmark-dir])
    ;; Compile
    (debug 1 "Preconditions OK, compiling 'main.rkt' ...")
    (unless (system "raco make main.rkt")
      (error 'trace-run:compile (format "Compilation failed for '~a/main.rkt', shutting down." typed-benchmark-dir)))
    ;; Run
    (debug 1 "Compilation succeeded, running 'main.rkt' ...")
    (match-define (list stdout stdin pid stderr ping) (process "racket main.rkt"))
    ;; Collect the results in a map
    (for ([line (in-lines stdout)])
      (cond
       [(bg-create? line) => add-contract/log]
       [(bg-apply?  line) => update-contract/log]
       [else (debug 2 "Ignoring line '~a'" line)]))
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr))
  contract-log)

;; Display aggregate stats about the contracts
;; (: show-contract (-> ContractUsageMap Void))
(define (show-contract cmap #:output-file [out-opt #f] #:valid-filename? [valid? #f])
  (debug 1 "Aggregating results")
  (define num-contracts (count-contracts cmap))
  (define num-checks (count-checks cmap))
  (define worst-boundaries (filter-worst-boundaries cmap #:valid-filename? valid?))
  (match-define (boundary wfrom wto wval wchecks) (car worst-boundaries))
  (define worst-total-contracts (count-contracts cmap #:from wfrom #:to wto))
  (define worst-total-checks (count-checks cmap #:from wfrom #:to wto))
  ;; Print the all things to file
  (when out-opt
    (with-output-to-file out-opt #:exists 'replace
      (lambda () (displayln DATA-FORMAT) (write (all-boundaries cmap)))))
  ;; Print the light things to the console.
  (printf "\nResults\n=======\n")
  (printf "Created ~a contracts\n" num-contracts)
  (printf "Checked contracts ~a times\n" num-checks)
  (printf "The worst boundary (~a -> ~a) created ~a contracts and caused ~a checks\n" wfrom wto worst-total-contracts worst-total-checks)
  (printf "Worst values, for each boundary:\n- ~a\n" (string-join (map format-boundary worst-boundaries) "\n- ")))

;; =============================================================================
;; Usage: `racket trace-run.rkt PROJECT-DIR`

(module+ main
  (require racket/cmdline)
  ;; -- parameters
  (define output-path (make-parameter #f))
  (define (set-param/contract val #:param p #:contract c)
    (unless (c val)
      (arg-error "Expected a '~a', got '~a'" c val))
    (p val))
  ;; -- command line
  (command-line
   #:program "trace-run"
   #:once-each
   [("-o" "--output") o-p
                      "A path to write output to"
                      (set-param/contract o-p #:param output-path #:contract path-string?)]
   #:args (parent-dir)
    (begin
      (ensure-hacked-racket)
      (ensure-preconditions parent-dir)
      (ensure-directory (format "~a/benchmark" parent-dir))
      (ensure-directory (format "~a/benchmark/base" parent-dir)
                        #:fill-with (in-glob (format "~a/base/*" parent-dir)))
      ;; -- run the project, collect contract information
      (define contract-set (collect-contract parent-dir))
      (define module-names (get-module-names parent-dir))
      ;; -- print a summary of the collected information
      (define (valid-filename? fn) (member fn module-names filename=?/adapted))
      (show-contract contract-set
                     #:output-file (output-path)
                     #:valid-filename? valid-filename?))))
