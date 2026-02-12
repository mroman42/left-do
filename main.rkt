#lang racket

(require leftdo/monad)
(require leftdo/normalized-distributions)
(require leftdo/normalization-almost-monad)
(require leftdo/left-do)



;; First list example.
;; Lists form a proper associative monad.
(define a (rDo List
  i <- (rDo List
         (list x y) <- (list (list 3 3) (list 4 5))
         v <- '(2 7 4)
         return (+ y v))
  _ <- '(2 3)
  return (* i i)))


(define b (lDo List
  i <- (lDo List
         (list x y) <- (list (list 3 3) (list 4 5))
         v <- '(2 7 4)
         return (+ y v))
  j <- '(2 3)
  return (* i i)))


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

;; Standard formulation of the Monty-Hall problem.
;; We pick the Left door.
;; The host announces that the middle door is empty.
;; Should we change doors?
(define r-monty-hall
  (rDo Norm
       car <- (uniform (list 'left 'middle 'right))
       choice <- (uniform (list 'left 'middle 'right))
       announce <- (host car choice)
       o1 <- (observe choice 'left)
       o2 <- (observe announce 'middle)
       return car))

(define l-monty-hall
  (lDo Norm
       car <- (uniform (list 'left 'middle 'right))
       choice <- (uniform (list 'left 'middle 'right))
       announce <- (host car choice)
       '() <- (observe choice 'left)
       '() <- (observe announce 'middle)
       return car))

(define monty-hall2
  (lDo Norm
       car <- (uniform (list 'left 'middle 'right))
       announce <- (host car 'middle)
       '() <- (observe announce 'left)
       return car))


;; Smoking causality example.
(define prevalence
  (distribution ['gene 1/3] ['nogene 2/3]))

(define (smokes hasgene?)
  (match hasgene?
    ['gene    (distribution ['smoker 3/4] ['nonsmoker 1/4])]
    ['nogene  (distribution ['smoker 1/4] ['nonsmoker 3/4])]))

(define (tar smokes?)
  (match smokes?
    ['smoker    (distribution ['tar 9/10] ['notar 1/10])]
    ['nonsmoker (distribution ['tar 1/10] ['notar 9/10])]))

(define (health hasgene? hastar?)
  (match hasgene?
    ['gene   (match hastar?
               ['tar    (distribution ['cancer 3/5] ['nocancer 2/5])]
               ['notar  (distribution ['cancer 3/10] ['nocancer 7/10])])]
    ['nogene (match hastar?
               ['tar    (distribution ['cancer 2/5] ['nocancer 3/5]  )]
               ['notar  (distribution ['cancer 2/10] ['nocancer 8/10])])]))


; Left-associating smoking problem.
(define l-smoking
  (lDo Norm
       has-gene <- prevalence
       is-smoker <- (smokes has-gene)
       '() <- (observe is-smoker 'smoker)
       has-tar <- (tar is-smoker)
       cancer <- (health has-gene has-tar)
       return cancer))

; Right-associating smoking problem.
(define r-smoking
  (rDo Norm
       has-gene <- prevalence
       is-smoker <- (smokes has-gene)
       '() <- (observe is-smoker 'smoker)
       has-tar <- (tar is-smoker)
       cancer <- (health has-gene has-tar)
       return cancer))

; Left-associating with normalization box smoking problem.
(define n-smoking
  (lDo Norm
       has-gene <- prevalence
       is-smoker <- (lDo Norm
                         is-smoker <- (smokes has-gene)
                         '() <- (observe is-smoker 'smoker)
                         return is-smoker)
       has-tar <- (tar is-smoker)
       cancer <- (health has-gene has-tar)
       return cancer))

;; Continuation monad example.
;(define (fib n)
;  (if (< n 2)
;      (rDo Cont
;           return n)
;      (rDo Cont
;           x <- (fib (- n 1))
;           y <- (fib (- n 2))
;           return (+ x y))))

