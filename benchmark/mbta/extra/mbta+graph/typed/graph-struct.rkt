#lang typed/racket/base

(provide (struct-out unweighted-graph))

(struct unweighted-graph
        ([get-vertices : (-> (Listof String))]
         [in-neighbors : (-> String (Sequenceof String))]))

