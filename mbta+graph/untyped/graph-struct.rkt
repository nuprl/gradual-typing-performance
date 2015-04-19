#lang racket/base

(provide (struct-out unweighted-graph))

(struct unweighted-graph
        (get-vertices
         in-neighbors))
