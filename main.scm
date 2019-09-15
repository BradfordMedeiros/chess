
(use-modules (srfi srfi-43))
(use-modules (ice-9 readline))
(use-modules (ice-9 match))

(define board 
  #(
    #("cb"  "hb" "bb" "qb" "kb" "bb" "hb" "cb")
    #("pb"  "pb" "pb" "pb" "pb" "pb" "pb" "pb")
    #( 0     0    0    0    0    0    0    0 )
    #( 0     0    0    0    0    0    0    0 )
    #( 0     0    0    0    0    0    0    0 )
    #( 0     0    0    0    0    0    0    0 )
    #("pw"  "pw" "pw" "pw" "pw" "pw" "pw" "pw")
    #("cw"  "hw" "bw" "kw" "qw" "bw" "hw" "cw")
  )
)

(define pawn-white-moves #(
  #(2 1 2)
  #(0 x 0)
  #(0 0 0)
))
(define pawn-black-moves (list->vector (reverse (vector->list pawn-white-moves))))
(define castle-moves #(
  #(0 4 0)
  #(4 x 4)
  #(0 4 0)
))
(define king-moves #(
  #(2 2 2)
  #(2 x 2)
  #(2 2 2)
))
(define queen-moves #(
  #(4 4 4)
  #(4 x 4) 
  #(4 4 4)
))
(define bishop-moves #(
  #(4 0 4)
  #(0 x 0)
  #(4 0 4)
))
(define horse-moves #(
  #(0 2 0 2 0)
  #(2 0 0 0 2)
  #(0 0 x 0 0)
  #(2 0 0 0 2)
  #(0 2 0 2 0)
))

(define symbol-to-moves '(
  ("pw"   pawn-white-moves  ) 
  ("pb"   pawn-black-moves  )
  ("cw"   castle-moves      )
  ("cb"   castle-moves      )
  ("kw"   king-moves        )
  ("kb"   king-moves        )
  ("qw"   queen-moves       )
  ("qb"   queen-moves       )
  ("bw"   bishop-moves      )
  ("bb"   bishop-moves      )
  ("hw"   horse-moves       )
  ("hb"   horse-moves       )
))




(define (merge-possible-moves current-board piece-moves xPos yPos)
   (display piece-moves)
)
(define (remaining-pieces board color)
  (define x 0)
  (do ((i 0 (+ i 1))) ((>= i (vector-length board)))
    (let ((vector-row (vector-ref board i)))
      (do ((j 0 (+ j 1))) ((>= j (vector-length vector-row) ))
        (let ((value (vector-ref vector-row j)))
          (cond 
            ((equal? value 0)(display ""))
            ((equal? (substring value 1 2) color) (set! x (+ x 1)))
            (#t (+ 0 0))
          )
        )
      )  
    )
  )
  x
)

(define value (remaining-pieces board "b"))
(display "value is: ")
(display value)
(display "\n")

(define (printboard board index) 
  (let ((length (vector-length board)))
    (if (< index length) 
        (begin
          (display index)
          (display " ")
          (display (vector-ref board index))
          (display "\n")
          (printboard board (+ index 1))
        ) 
    )
  )
)

(define (printinfo board) 
  (display "Current turn: ")
  (if is-white-turn (display "white") (display "black"))
  (display "\n\n")
  (printboard board 0)
)

(define is-white-turn #t)

(define (processGameMove move)
  ;(set! is-white-turn (not is-white-turn))
  (match (string-split move #\ )
    (("move" xPos yPos . rest) (begin 
      (display "xPos is: ")
      (display xPos)
      (display "\n")
    ))
    (("query" xPos yPos . rest) (begin 
      (display "xPos is: ")
      (display xPos)
      (display "\n")
      (merge-possible-moves board pawn-white-moves xPos yPos)
    ))
    (default (display "invalid query"))
  )
)

(define (inputToAction input)
  (cond 
    ((string=? input "info") (printinfo board))
    (else (processGameMove input))
  )
)

(define (gameloop)
  (inputToAction (readline))
  (gameloop)
)

(define pgm processGameMove)