(module data racket
  (struct snake (dir segs))
  (struct world (snake food))
  (struct posn (x y))
  
  (define (nelistof c) (cons/c c (listof c)))
  (define DIR/C (or/c "up" "down" "left" "right"))
  (define POSN/C (struct/c posn real? real?))
  (define SNAKE/C (struct/c snake DIR/C (nelistof POSN/C)))
  (define WORLD/C (struct/c world SNAKE/C POSN/C))
    
  (define (posn=? p1 p2)
    (and (= (posn-x p1) (posn-x p2))
         (= (posn-y p1) (posn-y p2))))  
  
  (provide [struct-out posn])
  
  (provide
   posn=?
   [struct-out snake]
   [struct-out world]
   DIR/C
   POSN/C
   SNAKE/C
   WORLD/C))
