#lang racket

(require leftdo/monad)
(require leftdo/subdistributions)
(require leftdo/left-do)
(require leftdo/normalized)


;; First list example.
(define a (rdo List
  i <- (rdo List
         (list x y) <- (list (list 3 3) (list 4 5))
         v <- '(2 7 4)
         pure (+ y v))
  _ <- '(2 3)
  pure (* i i)))


(define b (ldo List
  i <- (ldo List
         (list x y) <- (list (list 3 3) (list 4 5))
         v <- '(2 7 4)
         pure (+ y v))
  j <- '(2 3)
  pure (* i i)))


;; Monty-Hall problem
(define (host car choice)
  (match car
    ['left   (match choice
               ['left    (uniform (list 'middle 'right))]
               ['middle  (uniform (list 'right))]
               ['right   (uniform (list 'left))])]
    ['middle (match choice
               ['left   (uniform (list 'right))]
               ['middle (uniform (list 'left 'right))]
               ['right  (uniform (list 'left))])]
    ['right  (match choice
               ['left   (uniform (list 'middle))]
               ['middle (uniform (list 'left))]
               ['right  (uniform (list 'left 'right))])]))

(define (observe x y)
  (if (equal? x y)
      (uniform (list '()))
      (uniform (list))))

(define monty-hall
  (rdo Norm
       car <- (uniform (list 'left 'middle 'right))
       choice <- (uniform (list 'left 'middle 'right))
       announce <- (host car choice)
       o1 <- (observe choice 'left)
       o2 <- (observe announce 'middle)
       pure car))

;; Standard formulation of the Monty-Hall problem.
;; We pick the Left door.
;; The host announces that the middle door is empty.
;; Should we change doors?
(rdo Norm
     car <- (uniform (list 'left 'middle 'right))
     choice <- (uniform (list 'left 'middle 'right))
     announce <- (host car choice)
     o1 <- (observe choice 'left)
     o2 <- (observe announce 'middle)
     pure car)
