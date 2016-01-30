;; Data is a list of lists of boundary structures
;; There is one inner list for each boundary in the program
;; The boundary structures have 4 fields
;; - from-file : String
;; - to-file  : String
;; - val : String
;; - checks : Natural
((#s(boundary "server.rkt" "main.rkt" "server" 1)) (#s(boundary "constants.rkt" "server.rkt" "DATA" 1) #s(boundary "constants.rkt" "server.rkt" "PORT" 1)) (#s(boundary "constants.rkt" "client.rkt" "DATA" 1) #s(boundary "constants.rkt" "client.rkt" "PORT" 1)) (#s(boundary "client.rkt" "main.rkt" "client" 1)))