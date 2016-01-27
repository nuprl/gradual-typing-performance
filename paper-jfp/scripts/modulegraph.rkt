#lang typed/racket/base

;; Utilities for working with modules graphs.
;;
;; The source of truth are TiKZ'd module graphs
;; (because their layout requires human intervention)
;; so this file provides a (brittle) parser.

(provide
  from-directory
  ;; (-> Path ModuleGraph)
  ;; Parse a directory into a module graph.
  ;; Does not collect module dependency information.

  from-tex
  ;; (-> Path-String ModuleGraph)
  ;; Parse a tex file into a module graph

  module-names
  ;; (-> ModuleGraph (Listof String))
  ;; Return a list of all module names in the project

  path->project-name
  ;; (-> Path-String String)
  ;; Parse a project's name from a filename.

  project-name
  ;; (-> ModuleGraph String)
  ;; Get the project name direct from the modulegraph

  name->index
  ;; (-> ModuleGraph String Index)
  ;; Get the module's index into bitstrings

  index->name
  ;; (-> ModuleGraph Index String)

  requires
  ;; (-> ModuleGraph String (Listof String))
  ;; List of modules required by the given one

  (struct-out modulegraph)
  ModuleGraph
)

;; -----------------------------------------------------------------------------

(require
  racket/match
  (only-in racket/path file-name-from-path filename-extension)
  (only-in racket/sequence sequence->list)
  (only-in racket/string string-split string-trim)
)
(require/typed glob
  [glob (-> String (Listof String))])

;; =============================================================================
;; --- data definition: modulegraph

;; A module graph is represented as an adjacency list (all graphs are DAGs)
;; Invariant: names in the adjlist are kept in alphabetical order.
(struct modulegraph (
  [project-name : String]
  [adjlist : AdjList]) #:transparent)
(define-type AdjList (Listof (Listof String)))
(define-type ModuleGraph modulegraph)

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
  (for/list ([node+neighbors (in-list (modulegraph-adjlist mg))]
             #:when (member name (cdr node+neighbors)))
    (car node+neighbors)))

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

(: rkt-file? (-> Path Boolean))
(define (rkt-file? p)
  (regexp-match? #rx"\\.rkt$" (path->string p)))

(: from-directory (-> Path ModuleGraph))
(define (from-directory parent)
  (define name (path->project-name parent))
  ;; TODO works when we're in the paper/ directory, but nowhere else
  (define u-dir (build-path ".." name "untyped"))
  (unless (directory-exists? u-dir)
    (raise-user-error 'modulegraph (format "Failed to find source code for '~a', cannot summarize data" name)))
  ;; No edges, just nodes
  (: adjlist AdjList)
  (define adjlist (directory->adjlist u-dir))
  (modulegraph name adjlist))

;; Interpret a .tex file containing a TiKZ picture as a module graph
(: from-tex (-> Path-String ModuleGraph))
(define (from-tex filename)
  (define-values (path project-name) (ensure-tex filename))
  (call-with-input-file* filename
    (lambda ([port : Input-Port])
      (ensure-tikz port)
      (define-values (edge1 tex-nodes) (parse-nodes port))
      (define tex-edges (cons edge1 (parse-edges port)))
      (tex->modulegraph project-name tex-nodes tex-edges))))

;; Verify that `filename` is a tex file, return the name of
;; the project it describes.
(: ensure-tex (-> (U Path-String Path) (Values Path String)))
(define (ensure-tex filename)
  (define path (or (and (path? filename) filename)
                   (string->path filename)))
  (unless (bytes=? #"tex" (or (filename-extension path) #""))
    (parse-error "Cannot parse module graph from non-tex file '~a'" filename))
  ;; Remove anything past the first hyphen in the project name
  (define project-name (path->project-name path))
  (values path project-name))

;; Parse the project's name from a path
(: path->project-name (-> Path String))
(define (path->project-name path)
  (define p (or (file-name-from-path path) (error 'path->project-name)))
  (define without-ext
    (car (string-split (path->string p) ".")))
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
  (string=? "{};" (substring str (- N 3) N)))

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
(: tex->modulegraph (-> String (Listof texnode) (Listof texedge) ModuleGraph))
(define (tex->modulegraph project-name nodes edges)
  ;; Convert a TiKZ node id to a module name
  (: id->name (-> Index String))
  (define (id->name id)
    (or (for/or : (U #f String) ([tx (in-list nodes)])
          (and (= id (texnode-id tx))
               (texnode-name tx)))
        (error 'tex->modulegraph (format "Could not convert tikz node id ~a to a module name" id))))
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
  (modulegraph project-name untagged))

(: directory->adjlist (-> Path AdjList))
(define (directory->adjlist dir)
  (define src-path-str* (glob (format "~a/*.rkt" (path->string dir))))
  (define src-name*
    (for/list : (Listof String)
              ([path-str (in-list src-path-str*)])
      (path->project-name (string->path path-str))))
  (for/list ([path-str (in-list src-path-str*)]
             [name     (in-list src-name*)])
    (cons name
          (for/list : (Listof String)
                    ([name2 (in-list (parse-requires path-str))]
                     #:when (member name2 src-name*))
            name2))))

(define RX-REQUIRE #rx"require.*\"(.*)\\.rkt\"")

(: parse-requires (-> Path-String (Listof String)))
(define (parse-requires fname)
  (with-input-from-file fname
    (lambda ()
      (: match (Boxof String))
      (define match (box ""))
      (: regexp-match/set! (-> String (Option Void)))
      (define (regexp-match/set! str)
        (let ([m (regexp-match RX-REQUIRE str)])
          (if m
            (set-box! match (or (cadr m) (error 'parse-requires "internal")))
            #f)))
      (for/list : (Listof String)
                ([ln (in-lines)]
                 #:when (regexp-match/set! ln))
        (unbox match)))))

(define-type AdjList/Level (Listof (Listof (Listof String))))

(: group-by-level (-> AdjList AdjList/Level))
(define (group-by-level A)
  (let loop : AdjList/Level
            ([A : AdjList A]
             [level* : AdjList/Level '()]
             [seen* : (Listof String) '()])
    (if (null? A)
      level*
      (let* ([seen? : (-> String Boolean)
              (lambda ([s : String]) (and (member s seen*) #t))]
             [in-this-level
              (for/list : (Listof (Listof String))
                        ([name+req* (in-list A)]
                         #:when (andmap seen? (cdr name+req*)))
                name+req*)]
             [this-level-names : (Listof String)
               (map (inst car String (Listof String)) in-this-level)]
             [A2 (for/list : AdjList
                           ([name+req* (in-list A)]
                            #:when (not (member (car name+req*) this-level-names)))
                   name+req*)])

        (loop
          A2
          (cons in-this-level level*)
          (append this-level-names seen*))))))

;; Print a modulegraph for a project.
;; The layout should be approximately right
;;  (may need to bend edges & permute a row's nodes)
;;
;; TODO failed for snake (only 1 level!!!) pls debug
(: directory->tikz (-> Path Path-String Void))
(define (directory->tikz p out-file)
  (define N (path->project-name p))
  (define A (directory->adjlist p))
  (define MG (modulegraph N A))
  (define A/level (reverse (group-by-level A)))
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      (displayln "\\begin{tikzpicture}\n")
      (: name+tikzid* (Listof (Pairof String String)))
      (define name+tikzid*
       (apply append
        (for/list : (Listof (Listof (Pairof String String)))
                  ([group (in-list A/level)]
                   [g-id  (in-naturals)])
          (for/list : (Listof (Pairof String String))
                    ([name+req (in-list group)]
                     [n-id (in-naturals)])
            (define name (car name+req))
            (define tikzid (format "~a~a" g-id n-id))
            (define pos
              (cond
               [(and (zero? g-id) (zero? n-id)) ""]
               [(zero? n-id) (format "[left of=~a]" (decr-left tikzid))]
               [else (format "[below of=~a]" (decr-right tikzid))]))
            (printf "  \\node (~a) ~a {\\rkt{~a}{~a}};\n"
              tikzid pos (name->index MG name) name)
            (cons name tikzid)))))
      (newline)
      (: get-tikzid (-> String String))
      (define (get-tikzid name)
        (cdr (or (assoc name name+tikzid*) (error 'NONAME))))
      (for* ([group (in-list A/level)]
             [name+req* (in-list group)]
             [req (in-list (cdr name+req*))])
        (printf "  \\draw[->] (~a) -- (~a);\n"
          (get-tikzid (car name+req*))
          (get-tikzid req)))
      (displayln "\n\\end{tikzpicture}"))))

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
  (unless (= 1 (vector-length (current-command-line-arguments)))
    (raise-user-error "Usage: ./modulegraph.rkt PROJECT-NAME"))
  (define out-file "output.tex")
  (define u-path-str
    (string-append (vector-ref (current-command-line-arguments) 0) "/typed"))
  (directory->tikz (string->path u-path-str) out-file)
  (printf "Saved module graph to '~a'\n" out-file)
)

;; =============================================================================

(module+ test
  ;; -- Simple test, just make sure all module graphs parse.
  (require/typed glob [in-glob (-> String (Sequenceof String))])
  (: test-file (-> String Void))
  (define (test-file fn)
    (define mg (from-tex fn))
    (printf "Parsed '~a' from '~a'\n" mg fn))
  (for ([fname (in-glob "../module-graphs/*.tex")])
    (printf "TESTING ~a\n" fname)
    (test-file fname))

  ;; -- name->index
  ;; -- index->name
  ;; -- requires
)
