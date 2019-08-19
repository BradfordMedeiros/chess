#lang racket

(define is-white-turn #t)
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

(define bpawn-moves #(
  #(2 1 2)
  #(0 x 0)
  #(0 0 0)
))
(define wpawn-moves (list->vector (reverse (vector->list bpawn-moves))))

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
  ("pw" .  pawn-white-moves  ) 
  ("pb" .  pawn-black-moves  )
  ("cw" .  castle-moves      )
  ("cb" .  castle-moves      )
  ("kw" .  king-moves        )
  ("kb" .  king-moves        )
  ("qw" .  queen-moves       )
  ("qb" .  queen-moves       )
  ("bw" .  bishop-moves      )
  ("bb" .  bishop-moves      )
  ("hw" .  horse-moves       )
  ("hb" .  horse-moves       )
))


(define (merge-board current-board piece-movess)
   (print "todo")
)
(define (remaining-pieces board color)
   (print "not yet implemented")
)


