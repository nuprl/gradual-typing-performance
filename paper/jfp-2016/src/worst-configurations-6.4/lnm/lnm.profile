cpu time: 290 real time: 289 gc time: 50
Running time is 21.37% contracts
336/1570 ms

(->* ((sequence/c Real)) ((or/c #f (sequence/c Real))) any)      158.5 ms
(lib math/private/statistics/expected-values.rkt):7:9           
    mean                                                         158.5 ms

Index                                                            58.5 ms
bitstring.rkt:63:39                                             
    0                                                            47 ms
    1                                                            11.5 ms

(->i ((draw (-> (is-a?/c dc<%>) real? real? any)) (w real?)  ... 66.5 ms
<pkgs>/pict-lib/pict/main.rkt:72:3                              
    dc                                                           66.5 ms

(->* ((-> real? real? real?)) (#:color (or/c exact-integer?  ... 10 ms
<pkgs>/plot-lib/plot/private/deprecated/deprecated.rkt:22:3     
    contour                                                      10 ms

(->* (pict-convertible? pict-path? (-> pict? pict-path? (val ... 25 ms
<pkgs>/pict-lib/pict/private/utils.rkt:77:4                     
    pin-line                                                     25 ms

color%/c                                                         17 ms
<pkgs>/draw-lib/racket/draw.rkt:54:19                           
    color%                                                       17 ms

