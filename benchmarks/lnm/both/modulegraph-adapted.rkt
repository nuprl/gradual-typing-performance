#lang typed/racket/base

(require
  benchmark-util)

(require/typed/check "modulegraph.rkt"
  [#:struct modulegraph (
    [project-name : String]
    [adjlist : (Listof (Listof String))])]
  [project-name (-> ModuleGraph String)]
  [from-tex (-> Path-String ModuleGraph)]
  [module-names (-> ModuleGraph (Listof String))]
  [path->project-name (-> Path String)]
)
;; TODO can use opaque types instead?
(define-type ModuleGraph modulegraph)

(safe-and-unsafe-provide
  ModuleGraph
  project-name
  from-tex
  module-names
  path->project-name)
