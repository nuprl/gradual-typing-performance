#lang typed/racket/base

;; Utilities for working with modules graphs.
;;
;; The source of truth are TiKZ'd module graphs
;; (because their layout requires human intervention)
;; so this file provides a (brittle) parser.

(provide:
  (project-name->modulegraph (-> String ModuleGraph))
  (directory->modulegraph (-> Path-String ModuleGraph))
  ;; Parse a directory into a module graph.
  ;; Does not collect module dependency information.

  (tex->modulegraph (-> Path-String ModuleGraph))
  ;; Parse a tex file into a module graph

  (modulegraph->tex (-> ModuleGraph Output-Port Void))
  ;; Print a modulegraph to .tex

  (boundaries (-> ModuleGraph (Listof Boundary)))
  ;; Return a list of identifier-annotated edges in the program
  ;; Each boundary is a list (TO FROM PROVIDED)
  ;;  where PROVIDED is a list of type Provided (see the data definition below for 'struct provided')

  (boundary-to (-> Boundary String))
  (boundary-from (-> Boundary String))
  (boundary-provided* (-> Boundary (Listof Provided)))

  (in-edges (-> ModuleGraph (Sequenceof (Pairof String String))))
  ;; Iterate through the edges in a module graph.
  ;; Each edges is a pair of (TO . FROM)
  ;;  the idea is, each edges is a "require" from TO to FROM
  ;; Order of edges is unspecified.

  (module-names (-> ModuleGraph (Listof String)))
  ;; Return a list of all module names in the project

  (path->project-name (-> Path-String String))
  ;; Parse a project's name from a filename.

  (project-name (-> ModuleGraph String))
  ;; Get the project name direct from the modulegraph

  (name->index (-> ModuleGraph String Natural))
  ;; Get the module's index into bitstrings

  (index->name (-> ModuleGraph Natural String))

  (provides (-> ModuleGraph String (Listof String)))
  ;; List of modules that require the given one; i.e., modules the current provides to

  (requires (-> ModuleGraph String (Listof String)))
  ;; (-> ModuleGraph String (Listof String))
  ;; List of modules required by the given one

  (strip-suffix (-> Path-String String))
  ;; Remove the file extension from a path string

  (infer-project-dir (-> String Path-String))
  ;; Guess where the project is located in the GTP repo
)
(provide
  Boundary
  (struct-out modulegraph)
  ModuleGraph
  (struct-out provided)
  Provided
)

;; -----------------------------------------------------------------------------

(require
  glob/typed
  racket/match
  (only-in racket/system system)
  (only-in racket/port with-output-to-string)
  (only-in racket/list make-list last drop-right)
  (only-in racket/path file-name-from-path filename-extension)
  (only-in racket/sequence sequence->list)
  (only-in racket/string string-split string-contains? string-trim string-join)
)
(require/typed syntax/modcode
  (get-module-code
   (-> Path Any)))
(require/typed racket/string
  (string-contains? (-> String String Any)))

;; =============================================================================
;; --- data definition: modulegraph

;; A module graph is represented as an adjacency list (all graphs are DAGs)
;; Invariant: names in the adjlist are kept in alphabetical order.
(struct modulegraph (
  [project-name : String]
  [adjlist : AdjList]
  [src : (U #f Path-String)]
) #:transparent)
(define-type AdjList (Listof (Listof String)))
(define-type ModuleGraph modulegraph)

(: adjlist-add-edge (-> AdjList String String AdjList))
(define (adjlist-add-edge A* from to)
  (define found? : (Boxof Boolean) (box #f))
  (define res : AdjList
    (for/list : AdjList
              ([src+dst* (in-list A*)])
      (define src (car src+dst*))
      (define dst* (cdr src+dst*))
      (if (string=? from src)
        (begin
          (when (unbox found?)
            (raise-user-error 'adjlist-add-edge
              (format "Malformed adjacency list, node '~a' appears twice" from)))
          (set-box! found? #t)
          (if (member to dst*)
            ;; Already exists? That's fine
            src+dst*
            (list* src to dst*)))
        src+dst*)))
  (if (unbox found?)
    res
    (cons (list from to) res)))

(: in-edges (-> ModuleGraph (Listof (Pairof String String))))
(define (in-edges G)
  (for*/list : (Listof (Pairof String String))
             ([src+dst* (in-list (modulegraph-adjlist G))]
              [dst (in-list (cdr src+dst*))])
    (cons (car src+dst*) dst)))

;; Get the name of the project represented by a module graph
(: project-name (-> ModuleGraph String))
(define (project-name mg)
  (modulegraph-project-name mg))

;; Get the names of all modules in this graph's project
(: module-names (-> ModuleGraph (Listof String)))
(define (module-names mg)
  (for/list ([node+neighbors (in-list (modulegraph-adjlist mg))])
    (car node+neighbors)))

(: name->index (-> ModuleGraph String Natural))
(define (name->index mg name)
  (: maybe-i (U #f Natural))
  (define maybe-i
    ;; Simulated for/first
    (let loop ([i : Natural 0] [n+n (modulegraph-adjlist mg)])
      (if (string=? name (caar n+n))
        i
        (loop (add1 i) (cdr n+n)))))
  (or maybe-i
     (error 'name->index (format "Invalid module name ~a" name))))

(: index->name (-> ModuleGraph Natural String))
(define (index->name mg i)
  (car (list-ref (modulegraph-adjlist mg) i)))

(: requires (-> ModuleGraph String (Listof String)))
(define (requires mg name)
  (or
   (adjlist->dst* (modulegraph-adjlist mg) name)
   (raise-user-error 'modulegraph (format "Module '~a' is not part of graph '~a'" name mg))))

(: adjlist->dst* (-> AdjList String (U #f (Listof String))))
(define (adjlist->dst* adj name)
  (for/or : (U #f (Listof String))
          ([src+dst* (in-list adj)])
    (and
     (string=? name (car src+dst*))
     (cdr src+dst*))))

(: provides (-> ModuleGraph String (Listof String)))
(define (provides mg name)
  (adjlist->src* (modulegraph-adjlist mg) name))

(: adjlist->src* (-> AdjList String (Listof String)))
(define (adjlist->src* adj name)
  (for/list : (Listof String)
            ([node+neighbors : (Listof String) (in-list adj)]
             #:when (member name (cdr node+neighbors)))
    (car node+neighbors)))

;; =============================================================================
;; --- data definition: provided / required

(struct provided (
  [>symbol : Symbol] ;; Name of provided identifier
  [syntax? : Boolean] ;; If #t, identifier is exported syntax or renamed
  [history : (U #f (Listof Any))]
  ;; If #f, means id was defined in the module
  ;; Otherwise, is a flat list of id's history
) #:transparent )
(define-type Provided provided)

;; TODO should to/from by symbols?
(define-type Boundary (List String String (Listof Provided)))
(define boundary-to car)
(define boundary-from cadr)
(define boundary-provided* caddr)
;; For now, I guess we don't need a struct

;; Return a list of:
;;   (TO FROM PROVIDED)
;;  corresponding to the edges of modulegraph `G`.
;; This decorates each edges with the identifiers provided from a module
;;  and required into another.
(: boundaries (-> ModuleGraph (Listof Boundary)))
(define (boundaries G)
  ;; Reclaim source directory
  (define src (infer-untyped-dir
    (or (modulegraph-src G) (infer-project-dir (modulegraph-project-name G)))))
  (define name* (module-names G))
  (define from+provided**
    (for/list : (Listof (Pairof String (Listof Provided)))
              ([name (in-list name*)])
      ((inst cons String (Listof Provided))
        name
        (absolute-path->provided* (build-path src (string-append name ".rkt"))))))
  (for/list : (Listof Boundary)
            ([to+from (in-edges G)])
    (define to (car to+from))
    (define from (cdr to+from))
    (define maybe-provided* (assoc from from+provided**))
    (if maybe-provided*
      (list to from (cdr maybe-provided*))
      (raise-user-error 'boundaries (format "Failed to get provides for module '~a'" from)))))

(: absolute-path->provided* (-> Path (Listof Provided)))
(define (absolute-path->provided* p)
  (define cm (cast (compile (get-module-code p)) Compiled-Module-Expression))
  (define-values (p* s*) (module-compiled-exports cm))
  (append
   (parse-provided p*)
   (parse-provided s* #:syntax? #t)))

(define-type RawProvided
  (Pairof (U #f Integer)
    (Listof (List Symbol History))))
(define-type History (Listof Any)) ;; Lazy

(: parse-provided (->* [(Listof RawProvided)] [#:syntax? Boolean] (Listof Provided)))
(define (parse-provided p* #:syntax? [syntax? #f])
  (define p0
    (apply append
      (for/list : (Listof (Listof (List Symbol History)))
                ([p (in-list p*)] #:when (and (car p) (zero? (car p))))
        (define p+ (cdr p))
        (if (and (not (null? p+))
                 (symbol? (car p+)))
          (list p+)
          p+))))
  (for/list : (Listof Provided)
            ([p : (List Symbol History) (in-list p0)])
    (define name (car p))
    (define history (cadr p))
    (provided name syntax? (and (not (null? history)) history))))

;; -----------------------------------------------------------------------------
;; --- parsing TiKZ

(struct texnode (
  [id : Index]
  [index : Index]
  [name : String]
) #:transparent)
;; A `texedge` is a (Pairof Index Index)
(define-type texedge (Pairof Index Index))

(define-syntax-rule (parse-error msg arg* ...)
  (error 'modulegraph (format msg arg* ...)))

(: rkt-file? (-> Path-String Boolean))
(define (rkt-file? p)
  (regexp-match? #rx"\\.rkt$" (if (string? p) p (path->string p))))

(: project-name->modulegraph (-> String ModuleGraph))
(define (project-name->modulegraph name)
  (directory->modulegraph (infer-project-dir name)))

(: directory->modulegraph (-> Path-String ModuleGraph))
(define (directory->modulegraph dir)
  (define u-dir (infer-untyped-dir dir))
  ;; No edges, just nodes
  (: adjlist AdjList)
  (define adjlist (directory->adjlist u-dir))
  (modulegraph (path->project-name dir) adjlist dir))

(define GTP "gradual-typing-performance")

(: get-git-root (-> String))
(define (get-git-root)
  (define ok? : (Boxof Boolean) (box #t))
  (define outs
    (with-output-to-string
      (lambda ()
        (set-box! ok? (system "git rev-parse --show-toplevel")))))
  (when (not (string-contains? outs GTP))
    (printf "WARNING: unrecognized git repo '~a', was expecting '~a'. Proceeding anyway.\n" GTP))
  (if (unbox ok?)
    (string-trim outs)
    (raise-user-error 'modulegraph "Must be in `gradual-typing-performance` repo to use script")))

;; Blindly search for a directory called `name`.
(: infer-project-dir (-> String Path))
(define (infer-project-dir name)
  (define p-dir (build-path (get-git-root) "benchmarks" name))
  (if (directory-exists? p-dir)
    p-dir
    (raise-user-error 'modulegraph "Failed to find project directory for '~a', cannot summarize data" name)))

(: infer-untyped-dir (-> Path-String Path))
(define (infer-untyped-dir dir)
  (define u-dir (build-path dir "untyped"))
  (if (directory-exists? u-dir)
    u-dir
    (raise-user-error 'modulegraph "Failed to find untyped code for '~a', cannot summarize data" dir)))

;; Interpret a .tex file containing a TiKZ picture as a module graph
(: tex->modulegraph (-> Path-String ModuleGraph))
(define (tex->modulegraph filename)
  (define-values (path project-name) (ensure-tex filename))
  (call-with-input-file* filename
    (lambda ([port : Input-Port])
      (ensure-tikz port)
      (define-values (edge1 tex-nodes) (parse-nodes port))
      (define tex-edges (cons edge1 (parse-edges port)))
      (texnode->modulegraph project-name tex-nodes tex-edges))))

;; Verify that `filename` is a tex file, return the name of
;; the project it describes.
(: ensure-tex (-> Path-String (Values Path String)))
(define (ensure-tex filename)
  (define path (or (and (path? filename) filename)
                   (string->path filename)))
  (unless (bytes=? #"tex" (or (filename-extension path) #""))
    (parse-error "Cannot parse module graph from non-tex file '~a'" filename))
  ;; Remove anything past the first hyphen in the project name
  (define project-name (path->project-name path))
  (values path project-name))

;; Parse the project's name from a path
(: path->project-name (-> Path-String String))
(define (path->project-name ps)
  (define p : Path
    (cond
     [(path? ps) ps]
     [(string? ps) (string->path ps)]
     [else (raise-user-error 'path->project-name ps)]))
  (define s : String
    (path->string
      (or (file-name-from-path p)
          (raise-user-error 'path->project-name (format "Could not get filename from path '~a'" p)))))
  (define without-dir
    (last (string-split s "/")))
  (define without-ext
    (strip-suffix without-dir))
  (define without-hyphen
    (car (string-split without-ext "-")))
  without-hyphen)

;; Verify that the lines contained in `port` contain a TiKZ picture
;; Advance the port
(: ensure-tikz (-> Input-Port Void))
(define (ensure-tikz port)
  (define line (read-line port))
  (cond [(eof-object? line)
         ;; No more input = failed to read a module graph
         (parse-error "Input is not a TiKZ picture")]
        [(string=? "\\begin{tikzpicture}" (string-trim line))
         ;; Success! We have id'd this file as a TiKZ picture
         (void)]
        [else
         ;; Try again with what's left
         (ensure-tikz port)]))

;; Parse consecutive `\node` declarations in a TiKZ file,
;; ignoring blank spaces and comments.
(: parse-nodes (->* [Input-Port] [(Listof texnode)] (Values texedge (Listof texnode))))
(define (parse-nodes port [nodes-acc '()])
  (define raw-line (read-line port))
  (define line
    (if (eof-object? raw-line)
      ;; EOF here means there's no edges below
      (parse-error "Hit end-of-file while reading nodes. Module graphs must have edges.")
      (string-trim raw-line)))
  (cond
    [(< (string-length line) 4)
     ;; Degenerate line, can't contain anything useful
     (parse-nodes port nodes-acc)]
    [(equal? #\% (string-ref line 0))
     ;; Line is a comment, ignore
     (parse-nodes port nodes-acc)]
    [(string=? "\\node" (substring line 0 5))
     ;; Found node! Keep if it's a real node (not just for positioning), then continue parsing
     (define nodes-acc+
       (if (dummy-node? line)
         nodes-acc
         (cons (string->texnode line) nodes-acc)))
     (parse-nodes port nodes-acc+)]
    [(string=? "\\draw" (substring line 0 5))
     ;; Found edge, means this stage of parsing is over
     (values (string->texedge line) nodes-acc)]
    [else
     ;; Invalid input
     (parse-error "Cannot parse node from line '~a'" line)]))

;; Parse consecutive `\edge` declarations, ignore blanks and comments.
(: parse-edges (->* [Input-Port] [(Listof texedge)] (Listof texedge)))
(define (parse-edges port [edges-acc '()])
  (define raw-line (read-line port))
  (define line
    (if (eof-object? raw-line)
      ;; End of file; should have seen \end{tikzpicture}
      (parse-error "Parsing reached end-of-file before reading \end{tikzpicture}. Are you sure the input is valid .tex?")
      (string-trim raw-line)))
  (cond
    [(< (string-length line) 4)
     ;; Degenerate line, can't contain anything useful
     (parse-edges port edges-acc)]
    [(equal? #\% (string-ref line 0))
     ;; Line is a comment, ignore
     (parse-edges port edges-acc)]
    [(string=? "\\draw" (substring line 0 5))
     ;; Found edge! Parse and recurse
     (parse-edges port (cons (string->texedge line) edges-acc))]
    [(string=? "\\node" (substring line 0 5))
     ;; Should never see nodes here
     (parse-error "Malformed TiKZ file: found node while reading edges.")]
    [(string=? "\\end{tikzpicture}" line)
     ;; End of picture, we're done!
     edges-acc]
    [else
     ;; Invalid input
     (parse-error "Cannot parse edge from line '~a'" line)]))

;; For parsing nodes:
;;   \node (ID) [pos]? {\rkt{ID}{NAME}};
(define NODE_REGEXP
  #rx"^\\\\node *\\(([0-9]+)\\) *(\\[.*\\])? *\\{\\\\rkt\\{([0-9]+)\\}\\{(.+)\\}\\};$")
;; For parsing edges
;;   \draw[style]? (ID) edge (ID);
(define EDGE_REGEXP
  #rx"^\\\\draw\\[.*\\]? *\\(([0-9]+)\\)[^(]*\\(([0-9]+)\\);$")

;; Parsing
(: string->index (-> String Index))
(define (string->index str)
  (cast (string->number str) Index))

;; Check if a line represents a real node, or is just for positioning
(: dummy-node? (-> String Boolean))
(define (dummy-node? str)
  (define N (string-length str))
  (and (>= N 3)
       (string=? "{};" (substring str (- N 3) N))))

;; Parse a string into a texnode struct.
(: string->texnode (-> String texnode))
(define (string->texnode str)
  (define m (regexp-match NODE_REGEXP str))
  (match m
    [(list _ id _ index name)
     #:when (and id index name)
     (texnode (or (string->index id) (parse-error "Could not parse integer from node id '~a'" id))
              (or (string->index index) (parse-error "Could not parse integer from node index '~a'" index))
              name)]
    [else
     (parse-error "Cannot parse node declaration '~a'" str)]))

;; Parse a string into a tex edge.
;; Edges are represented as cons pairs of their source and destination.
;; Both source and dest. are represented as indexes.
(: string->texedge (-> String texedge))
(define (string->texedge str)
  (define m (regexp-match EDGE_REGEXP str))
  (match m
    [(list _ id-src id-dst)
     #:when (and id-src id-dst)
     ((inst cons Index Index)
           (string->index id-src)
           (string->index id-dst))]
    [else
     (parse-error "Cannot parse edge declaration '~a'" str)]))

;; Convert nodes & edges parsed from a .tex file to a modulegraph struct
(: texnode->modulegraph (-> String (Listof texnode) (Listof texedge) ModuleGraph))
(define (texnode->modulegraph project-name nodes edges)
  ;; Convert a TiKZ node id to a module name
  (: id->name (-> Index String))
  (define (id->name id)
    (or (for/or : (U #f String) ([tx (in-list nodes)])
          (and (= id (texnode-id tx))
               (texnode-name tx)))
        (error 'texnode->modulegraph (format "Could not convert tikz node id ~a to a module name" id))))
  ;; Create an adjacency list by finding the matching edges for each node
  (: adjlist (Listof (Pairof (Pairof Index String) (Listof String))))
  (define adjlist
    (for/list
      ([tx : texnode (in-list nodes)])
      (: hd (Pairof Index String))
      (define hd (cons (texnode-index tx) (texnode-name tx)))
      (: rest (Listof String))
      (define rest
        (for/list
          ([src+dst : texedge (in-list edges)]
           #:when (= (texnode-id tx) (car src+dst)))
              (id->name (cdr src+dst))))
        ((inst cons (Pairof Index String) (Listof String))
         hd rest)))
  ;; Alphabetically sort the adjlist, check that the indices match the ordering
  ;; Need to append .rkt, else things like (string< "a-base" "a") fail. They should pass...
  (: get-key (-> (Pairof (Pairof Index String) (Listof String)) String))
  (define (get-key x)
    (string-append (cdar x) ".rkt"))
  (define sorted ((inst sort (Pairof (Pairof Index String) (Listof String)) String)
    adjlist string<? #:key get-key))
  (unless (equal? (for/list : (Listof Index)
                    ([x (in-list sorted)])
                    (caar x))
                  (sequence->list (in-range (length sorted))))
    (parse-error "Indices do not match alphabetical ordering on module names. Is the TiKZ graph correct?\n    Source: '~a'\n" (for/list : (Listof Any) ([x (in-list sorted)]) (car x))))
  ;; Drop the indices
  (define untagged : (Listof (Listof String))
    (for/list ([tag+neighbors (in-list sorted)])
      (cons (cdar tag+neighbors) (cdr tag+neighbors))))
  (modulegraph project-name untagged #f))

(: directory->adjlist (-> Path AdjList))
(define (directory->adjlist dir)
  (define abs-path* (glob (format "~a/*.rkt" (path->string dir))))
  (define src-name*
    (for/list : (Listof String)
              ([path-str (in-list abs-path*)])
      (strip-suffix (strip-directory (string->path path-str)))))
  ;; Build modulegraph
  (for/list : AdjList
            ([abs-path (in-list abs-path*)]
             [src-name (in-list src-name*)])
    (cons src-name
          (for/list : (Listof String)
                    ([mod-abspath (in-list (absolute-path->imports abs-path))]
                     #:when (member (strip-suffix mod-abspath) src-name*))
            (strip-suffix mod-abspath)))))

(: absolute-path->imports (-> Path-String (Listof Path)))
(define (absolute-path->imports ps)
  (define p (if (path? ps) ps (string->path ps)))
  (define mc (cast (compile (get-module-code p)) Compiled-Module-Expression))
  (for/fold : (Listof Path)
            ([acc : (Listof Path) '()])
            ([mpi (in-list (apply append (module-compiled-imports mc)))])
    (if (module-path-index? mpi)
      (let-values (((name _2) (module-path-index-split mpi)))
        (if (string? name)
          (cons (string->path name) acc)
          acc))
      acc)))

(define RX-REQUIRE #rx"require.*\"(.*)\\.rkt\"")

;; Sort an adjacency list in order of transitive indegree, increasing.
;; Results are grouped by indegree, i.e.
;; - 1st result = list of 0-indegree nodes
;; - 2nd result = list of 1-indegree nodes
;; - ...
(: topological-sort (-> AdjList (Listof (Listof String))))
(define (topological-sort adj)
  (: indegree-map (HashTable String Integer))
  (define indegree-map
    (make-hash (for/list : (Listof (Pairof String Integer))
                         ([src+dst* (in-list adj)])
                 (cons (car src+dst*) (length (cdr src+dst*))))))
  (reverse
    (let loop ([acc : (Listof (Listof String)) '()])
      (cond
       [(zero? (hash-count indegree-map))
        acc]
       [else
        (define zero-indegree*
          (for/list : (Listof String)
                    ([(k v) (in-hash indegree-map)]
                     #:when (zero? v)) k))
        (for ([k (in-list zero-indegree*)])
          (hash-remove! indegree-map k)
          (define src* (adjlist->src* adj k))
          (for ([src (in-list src*)])
            (hash-set! indegree-map src
              (- (hash-ref indegree-map src (lambda () -1)) 1))))
        (loop (cons (sort zero-indegree* string<?) acc))]))))

;; Print a modulegraph for a project.
;; The layout should be approximately right
;;  (may need to bend edges & permute a row's nodes)
(: directory->tikz (-> Path Path-String Void))
(define (directory->tikz p out-file)
  (define MG (directory->modulegraph p))
  (with-output-to-file out-file #:exists 'replace
    (lambda () (modulegraph->tex MG (current-output-port)))))

(: modulegraph->tex (-> ModuleGraph Output-Port Void))
(define (modulegraph->tex MG out)
  (define tsort (topological-sort (modulegraph-adjlist MG)))
  (parameterize ([current-output-port out])
    (displayln "\\begin{tikzpicture}\n")
    ;; -- draw nodes
    (: name+tikzid* (Listof (Pairof String String)))
    (define name+tikzid*
     (apply append
      (for/list : (Listof (Listof (Pairof String String)))
                ([group (in-list tsort)]
                 [g-id  (in-naturals)])
        (for/list : (Listof (Pairof String String))
                  ([name (in-list group)]
                   [n-id (in-naturals)])
          (define tikzid (format "~a~a" g-id n-id))
          (define pos
            (cond
             [(and (zero? g-id) (zero? n-id)) ""]
             [(zero? n-id) (format "[left of=~a,xshift=-2cm]" (decr-left tikzid))]
             [else (format "[below of=~a,yshift=-1cm]" (decr-right tikzid))]))
          (printf "  \\node (~a) ~a {\\rkt{~a}{~a}};\n"
            tikzid pos (name->index MG name) name)
          (cons name tikzid)))))
    (newline)
    ;; -- draw edges
    (: get-tikzid (-> String String))
    (define (get-tikzid name)
      (cdr (or (assoc name name+tikzid*) (error 'NONAME))))
    (for* ([group (in-list tsort)]
           [name (in-list group)]
           [req (in-list (requires MG name))])
      (printf "  \\draw[->] (~a) -- (~a);\n"
        (get-tikzid name)
        (get-tikzid req)))
    (displayln "\n\\end{tikzpicture}")))

(: decr-right (-> String String))
(define (decr-right str)
  (decr-str str #f #t))

(: decr-left (-> String String))
(define (decr-left str)
  (decr-str str #t #f))

(: decr-str (-> String Boolean Boolean String))
(define (decr-str str left? right?)
  (define left-char (string-ref str 0))
  (define right-char (string-ref str 1))
  (string (if left? (decr-char left-char) left-char)
          (if right? (decr-char right-char) right-char)))

(: decr-char (-> Char Char))
(define (decr-char c)
  (integer->char (sub1 (char->integer c))))

;; =============================================================================

(module+ main
  (define-syntax-rule (usage-error)
    (raise-user-error "Usage: ./modulegraph.rkt PROJECT-NAME"))
  (unless (= 1 (vector-length (current-command-line-arguments)))
    (usage-error))
  (define out-file "output.tex")
  (define name (vector-ref (current-command-line-arguments) 0))
  (define p (if (directory-exists? name)
              (string->path name)
              (infer-project-dir name)))
  (directory->tikz p out-file)
  (printf "Saved module graph to '~a'\n" out-file)
)

(: strip-suffix (-> Path-String String))
(define (strip-suffix p)
  (define p+ (if (path? p) p (string->path p)))
  (path->string (path-replace-suffix p+ "")))

(: strip-directory (-> Path-String String))
(define (strip-directory ps)
  (define p (if (path? ps) ps (string->path ps)))
  (path->string (assert (last (explode-path p)) path?)))

;; =============================================================================

(module+ test
  (require
    racket/port
    typed/rackunit)

  (define SAMPLE-MG-FILE "test/sample-modulegraph.tex")
  (define SAMPLE-MG-PROJECT-NAME "echo")

  ;; -- Test parsing

  ;; -- Parse all module graphs
  (define MGf (tex->modulegraph SAMPLE-MG-FILE))
  (define MGd (project-name->modulegraph SAMPLE-MG-PROJECT-NAME))

  ;; --- project-name
  (check-false (string-contains? (project-name MGf) "-"))
  (check-equal? (project-name MGf) "sample") ;; Hyphen is stripped

  (check-equal? (project-name MGd) SAMPLE-MG-PROJECT-NAME)

  ;; -- module-names
  (check-equal? (sort (module-names MGf) string<?)
               '("collide" "const" "cut-tail" "data" "handlers" "main" "motion" "motion-help"))
  (check-equal? (module-names MGd) '("client" "constants" "main" "server"))

  ;; -- name->index
  (check-equal? (name->index MGf "collide") 0)
  (check-equal? (name->index MGf "handlers") 4)

  (check-equal? (name->index MGd "main") 2)

  ;; -- index->name
  (check-equal? (index->name MGf 4) "handlers")
  (check-equal? (index->name MGf (assert (- (length (module-names MGf)) 3) index?)) "main")

  ;; -- provides
  (check-equal? (provides MGf "data") '("collide" "const" "cut-tail" "handlers" "main" "motion-help" "motion"))
  (check-equal? (provides MGf "main") '())

  ;; -- requires
  (check-equal? (requires MGf "data") '())
  (check-equal? (requires MGf "main") '("motion" "handlers" "const" "data"))

  ;; -- rkt-file?
  (check-true (rkt-file? "foo.rkt"))
  (check-true (rkt-file? "bar.rkt"))
  (check-true (rkt-file? ".rkt"))

  (check-false (rkt-file? "rktd"))
  (check-false (rkt-file? "yolo"))
  (check-false (rkt-file? ""))

  ;; -- adjlist
  (check-equal? (modulegraph-adjlist MGd)
    '(("client" "constants") ("constants") ("main" "server" "client") ("server" "constants")))

  (: lex-pair<? (-> (Pairof String String) (Pairof String String) Boolean))
  (define (lex-pair<? a b)
    (or (string<? (car a) (car b))
        (and (string=? (car a) (car b))
             (string<? (cdr a) (cdr b)))))

  (check-equal?
    (sort (sequence->list (in-edges MGf)) lex-pair<?)
    '(("collide" . "const") ("collide" . "data") ("const" . "data") ("cut-tail" . "data") ("handlers" . "collide") ("handlers" . "data") ("handlers" . "motion") ("main" . "const") ("main" . "data") ("main" . "handlers") ("main" . "motion") ("motion" . "const") ("motion" . "data") ("motion" . "motion-help") ("motion-help" . "cut-tail") ("motion-help" . "data")))

  (check-equal?
    (sort (sequence->list (in-edges MGd)) lex-pair<?)
    '(("client" . "constants") ("main" . "client") ("main" . "server") ("server" . "constants")))

  (let ([bd (boundaries MGd)])
    (check-equal? (length bd) (length (sequence->list (in-edges MGd))))
    (let ([b1 (car bd)])
      (check-equal? (car b1) "client")
      (check-equal? (cadr b1) "constants")
      (let ([p* (caddr b1)])
        (check-equal? (length p*) 2)
        (check-equal? (provided->symbol (car p*)) 'DATA)
        (check-equal? (provided->symbol (cadr p*)) 'PORT)))
    (let ([b3 (caddr bd)])
      (check-equal? (car b3) "main")
      (check-equal? (cadr b3) "client")
      (let ([p* (caddr b3)])
        (check-equal? (length p*) 1)
        (check-equal? (provided->symbol (car p*)) 'client))))

  ;; -- directory->modulegraph
  ;; -- tex->modulegraph
  ;; TESTED ABOVE

  ;; -- infer-untyped-dir
  (check-equal?
    (infer-untyped-dir (infer-project-dir SAMPLE-MG-PROJECT-NAME))
    (build-path (get-git-root) "benchmarks" SAMPLE-MG-PROJECT-NAME "untyped"))
  (check-exn exn:fail:user?
    (lambda () (infer-project-dir "nasdhoviwr")))
  (check-exn exn:fail:user?
    (lambda () (infer-untyped-dir "nasdhoviwr")))

  ;; -- ensure-tex
  (define-syntax-rule (check-ensure-tex [in o1 o2] ...)
    (begin
      (let-values ([(p s) (ensure-tex in)])
        (check-equal? p o1)
        (check-equal? s o2)) ...))
  (check-ensure-tex
    ["a.tex" (string->path "a.tex") "a"]
    ["bar.tex" (string->path "bar.tex") "bar"]
    ["bar-3.tex" (string->path "bar-3.tex") "bar"])

  (check-exn exn:fail?
    (lambda ()
      (let-values (((a b) (ensure-tex "foo"))) (void))))
  (check-exn exn:fail?
    (lambda ()
      (let-values (((a b) (ensure-tex "foo.rkt"))) (void))))

  ;; -- path->project-name
  (check-equal? (path->project-name "foo-bar-baz.rkt") "foo")
  (check-equal? (path->project-name (string->path "x-6.3")) "x")
  (check-equal? (path->project-name "yes/no/maybe-so.out") "maybe")

  ;; -- ensure-tikz
  (check-equal?
    (call-with-input-string "\\begin{tikzpicture}"
      ensure-tikz)
    (void))
  (check-equal?
    (call-with-input-string "\nblahblahblah\n\n\\begin{tikzpicture}"
      ensure-tikz)
    (void))

  (check-exn exn:fail?
    (lambda ()
      (call-with-input-string ""
        ensure-tikz)))
  (check-exn exn:fail?
    (lambda ()
      (call-with-input-string "nope"
        ensure-tikz)))

  ;; -- strip-suffix
  (check-equal? (strip-suffix "file.txt") "file")
  (check-equal? (strip-suffix "yo/lo.com") "yo/lo")
  (check-equal? (strip-suffix "cant.stop.now") "cant.stop")

  ;; -- strip-directory
  (check-equal? (strip-directory "file.txt") "file.txt")
  (check-equal? (strip-directory "yo/lo.com") "lo.com")
  (check-equal? (strip-directory "cant.stop.now") "cant.stop.now")
  (check-equal? (strip-directory "cant/stop/now") "now")

  ;; -- parse-nodes TODO
  ;; -- parse-edges TODO

  ;; -- string->index
  (check-equal? (string->index "2") 2)
  (check-equal? (string->index "6789") 6789)

  (check-exn exn:fail:contract?
    (lambda () (string->index "yes")))
  (check-exn exn:fail:contract?
    (lambda () (string->index "-1")))

  ;; -- dummy-node?
  (check-true (dummy-node? "{};"))
  (check-true (dummy-node? "yolo {};"))
  (check-true (dummy-node? "some fist things {}; {}; yeah {};"))

  (check-false (dummy-node? ""))
  (check-false (dummy-node? "{}"))
  (check-false (dummy-node? "{}; and1"))

  ;; -- string->texnode
  (define-syntax-rule (check-string->texnode [str n] ...)
    (begin
      (let ([n2 (string->texnode str)])
        (check-true (not (eq? #f n2)))
        (for ([acc (in-list (list texnode-id texnode-index texnode-name))])
          (check-equal? (acc n2) (acc n)))) ...))
  (check-string->texnode
   ["\\node (0) {\\rkt{0}{hi}};" (texnode 0 0 "hi")]
   ["\\node (10) [some crap] {\\rkt{777}{hi.rkt}};" (texnode 10 777 "hi.rkt")])

  (check-exn exn:fail?
    (lambda ()
      (string->texnode "")))
  (check-exn exn:fail?
    (lambda ()
      (string->texnode "weeeeeepa")))

  (check-equal?
    (topological-sort '(("client" "constants") ("constants") ("main" "server" "client") ("server" "constants")))
    '(("constants") ("client" "server") ("main")))

  (check-equal?
    (topological-sort (modulegraph-adjlist (project-name->modulegraph "suffixtree")))
    '(("data") ("label") ("structs") ("ukkonen") ("lcs") ("main")))

  (check-equal?
    (modulegraph-adjlist (project-name->modulegraph "synth"))
    '(("array-broadcast" "data" "array-utils" "array-struct") ("array-struct" "data" "array-utils") ("array-transform" "data" "array-utils" "array-broadcast" "array-struct") ("array-utils") ("data") ("drum" "data" "synth" "array-transform" "array-utils" "array-struct") ("main" "synth" "mixer" "drum" "sequencer") ("mixer" "array-broadcast" "array-struct") ("sequencer" "mixer" "synth" "array-transform" "array-struct") ("synth" "array-utils" "array-struct")))

  (check-equal?
    (topological-sort (modulegraph-adjlist (project-name->modulegraph "synth")))
    '(("array-utils" "data") ("array-struct") ("array-broadcast" "synth") ("array-transform" "mixer") ("drum" "sequencer") ("main")))


  (check-equal?
    (map (lambda ([b : Boundary])
           (list (boundary-to b)
                 (boundary-from b)
                 (map provided->symbol (boundary-provided* b))))
         (boundaries MGd))
    '(("client" "constants" (DATA PORT)) ("main" "server" (server)) ("main" "client" (client)) ("server" "constants" (DATA PORT))))

  ;; -- string->texedge TODO
  ;; -- texnode->modulegraph TODO
  ;; -- directory->adjlist TODO
)

