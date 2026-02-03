#lang racket

(require rackunit)
(require leftdo/monad)
(require leftdo/subdistributions)
(require leftdo/normalized)
(require leftdo/left-do)

(define (observe x y)
  (if (equal? x y)
      (uniform (list '()))
      (uniform (list))))

;; Partial stochastic
;; f : Dir -> Color x Color
(define (f x)
  (match x
    ['left  (distribution [(list 'green 'red) 2/4] [(list 'blue 'green) 1/4] [(list 'blue 'red) 1/4])]
    ['right (uniform (list))]))

;; Partial stochastic
;; g : Dir -> Dir
(define (g x)
  (match x
    ['left (uniform (list))]
    ['right (distribution ['left 1/5] ['right 4/5])]))

;; Stochastic
;; h : Color x Dir -> Color
(define (h y x)
  (match x
    ['left  (match y
              ['blue  (distribution ['green 2/4] ['blue 1/4] ['red 1/4])]
              ['green (distribution ['red 2/3] ['green 1/3])]
              ['red   (distribution ['green 1/7] ['blue 1/7])])]
    ['right (distribution ['green 1/3] ['red 2/3])]))


;; Left-hand side
(define (eq-lhs a b c)
  (lDo Norm
       (list x y) <- (f a)
       u <- (g b)
       v <- (h y c)
       return (list x u v)))

(define (eq-rhs a b c)
  (lDo Norm
       (list x y) <- (f a)
       v <- (h y c)
       u <- (g b)
       return (list x u v)))

;; These are equal, apparently.