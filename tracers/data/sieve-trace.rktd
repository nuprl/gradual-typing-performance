;; Data is a list of lists of boundary structures
;; There is one inner list for each boundary in the program
;; The boundary structures have 4 fields
;; - from-file : String
;; - to-file  : String
;; - val : String
;; - checks : Natural
((#s(boundary "stream>" "main.rkt" "(#<syntax:/home/ben/code/racket/benchmark/gradual-typing-performance/sieve/benchmark/variation11/main.rkt:9:12" 0)) (#s(boundary "streams.rkt" "main.rkt" "stream?" 201692320) #s(boundary "streams.rkt" "main.rkt" "stream-unfold" 201672318) #s(boundary "streams.rkt" "main.rkt" "make-stream" 100856161) #s(boundary "streams.rkt" "main.rkt" "stream-get" 1) #s(boundary "streams.rkt" "main.rkt" "stream-rest" 0) #s(boundary "streams.rkt" "main.rkt" "stream-take" 0) #s(boundary "streams.rkt" "main.rkt" "stream-first" 0)))