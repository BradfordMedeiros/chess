(use-modules (srfi srfi-43))
(use-modules (ice-9 readline))
(use-modules (ice-9 match))
(use-modules (ice-9 eval-string))

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
(define no-moves #(
  #(x)
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
  (0      no-moves          )
))

(define (get-moves-center piece-moves)
  (define center (cons -1 -1))
  (do ((row 0 (+ row 1))) ((>= row (vector-length piece-moves)))
    (let ((current-row (vector-ref piece-moves row)))
      (do ((col 0 (+ col 1))) ((>= col (vector-length current-row)))
        (if (equal? 'x (vector-ref current-row col)) 
          (set! center (cons row col))
        )
      )
    )
  )
  center
)

(define (get-value-from-moves piece-moves row col yPos xPos defaultValue)
  (define centerMoves (get-moves-center piece-moves))
  (let ((relativeY (- row (- yPos (cdr centerMoves)))) (relativeX (- col (- xPos (car centerMoves)))) (movesHeight (vector-length piece-moves)) (movesWidth (vector-length (vector-ref piece-moves 0))))
    (cond
      ((< relativeX 0) defaultValue)
      ((< relativeY 0) defaultValue)
      ((>= relativeY movesHeight) defaultValue)
      ((>= relativeX movesWidth) defaultValue)
      (#t (vector-ref (vector-ref piece-moves relativeY) relativeX))
    )
  )
)

(define (merge-possible-moves current-board piece-moves yPos xPos)
  (define newboard #())
  (define pieceHeight (vector-length piece-moves))
  (define pieceWidth  (vector-length (vector-ref piece-moves 0)))
  (do ((row 0 (+ row 1))(newRow #())) ((>= row (vector-length current-board)))
    (let ((current-row (vector-ref current-board row)))
      (define newRow #())
      (do ((col 0 (+ col 1))) ((>= col (vector-length current-row)))
        (set! newRow (vector-append newRow (make-vector 1 (get-value-from-moves piece-moves row col yPos xPos (vector-ref current-row col)))))
      )
      (set! newboard (vector-append newboard (make-vector 1 newRow)))
    )
  )
  newboard
)

(define (piece-at-index board row col) (vector-ref (vector-ref board row) col))
(define (get-moves-for-piece piece) (cadr (assoc piece symbol-to-moves)))
(define (moves-at-index board row col) (get-moves-for-piece (piece-at-index board row col)))

(define (remaining-pieces board color)
  (define numPieces 0)
  (do ((i 0 (+ i 1))) ((>= i (vector-length board)))
    (let ((vector-row (vector-ref board i)))
      (do ((j 0 (+ j 1))) ((>= j (vector-length vector-row) ))
        (let ((value (vector-ref vector-row j)))
          (cond 
            ((equal? value 0)(display ""))
            ((equal? (substring value 1 2) color) (set! numPieces (+ numPieces 1)))
            (#t (+ 0 0))
          )
        )
      )  
    )
  )
  numPieces
)

(define (moveboard fromXPos fromYPos toXPos toYPos)
  board
)

(define (printboard board index) 
  (let ((length (vector-length board)))
    (if (< index length) 
        (begin
          (display index)(display " ")(display (vector-ref board index))(display "\n")
          (printboard board (+ index 1))
        ) 
    )
  )
)

(define (processGameMove move)
  (match (string-split move #\ )
    (("move" fromXPos fromYPos toXPos toYPos . rest) 
      (moveboard fromXPos fromYPos toXPos toYPos)
    )
    (("query" xPos yPos . rest) (begin 
      (define xPosInt (string->number xPos))
      (define yPosInt (string->number yPos))
      (define moves (moves-at-index board xPosInt yPosInt))
      (define moveSet (eval moves (interaction-environment)))
      (merge-possible-moves board moveSet xPosInt yPosInt)
    ))
    (default "invalid query")
  )
)

(define (inputToAction input)
  (cond 
    ((string=? input "info") (printinfo board))
    (else (printboard (processGameMove input) 0))
  )
)

(define is-white-turn #t)

(define (printinfo board) 
  (display "Current turn: ")
  (if is-white-turn (display "white\n\n") (display "black\n\n"))
  (printboard board 0)
)

(define (gameloop)
  (inputToAction (readline))
  (gameloop)
)



;(define newboard (merge-possible-moves board horse-moves 0 1))
;(printinfo newboard)
;(gameloop)

