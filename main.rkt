#lang racket

(require leftdo/monad)
(require leftdo/subdistributions)
(require leftdo/normalized)
(require leftdo/left-do)



;; First list example.
;; Lists form a proper associative monad.
(define a (rdo List
  i <- (rdo List
         (list x y) <- (list (list 3 3) (list 4 5))
         v <- '(2 7 4)
         return (+ y v))
  _ <- '(2 3)
  return (* i i)))


(define b (ldo List
  i <- (ldo List
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

(define monty-hall
  (rdo Norm
       car <- (uniform (list 'left 'middle 'right))
       choice <- (uniform (list 'left 'middle 'right))
       announce <- (host car choice)
       o1 <- (observe choice 'left)
       o2 <- (observe announce 'middle)
       return car))


;; Standard formulation of the Monty-Hall problem.
;; We pick the Left door.
;; The host announces that the middle door is empty.
;; Should we change doors?
(rdo Norm
     car <- (uniform (list 'left 'middle 'right))
     choice <- (uniform (list 'left 'middle 'right))
     announce <- (host car choice)
     '() <- (observe choice 'left)
     '() <- (observe announce 'middle)
     return car)

(ldo Norm
     car <- (uniform (list 'left 'middle 'right))
     choice <- (uniform (list 'left 'middle 'right))
     announce <- (host car choice)
     '() <- (observe choice 'left)
     '() <- (observe announce 'middle)
     return car)


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

(ldo Norm
     has-gene <- prevalence
     is-smoker <- (smokes has-gene)
     o1 <- (observe is-smoker 'smoker)
     has-tar <- (tar is-smoker)
     cancer <- (health has-gene has-tar)
     return cancer)

(rdo Norm
     has-gene <- prevalence
     is-smoker <- (smokes has-gene)
     o1 <- (observe is-smoker 'smoker)
     has-tar <- (tar is-smoker)
     cancer <- (health has-gene has-tar)
     return cancer)