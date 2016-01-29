;; Data is a list of lists of boundary structures
;; There is one inner list for each boundary in the program
;; The boundary structures have 4 fields
;; - from-file : String
;; - to-file  : String
;; - val : String
;; - checks : Natural
((#s(boundary "morse-code-table.rkt" "morse-code-strings.rkt" "char-table" 1642120)) (#s(boundary "morse-code-strings.rkt" "main.rkt" "string->morse" 186050)) (#s(boundary "levenshtein.rkt" "main.rkt" "string-levenshtein" 186050)))