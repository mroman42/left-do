#lang racket

;; monad.rkt

;; This file provides the interface for monads (return, bind) and
;; implementations for multiple basic monads (List, Identity, Maybe, ...).
;; The distribution monad is implemented separately (subdistributions.rkt).

(struct monad (return bind))

;; List monad.
(define List
  (monad
   (λ (x) (list x))
   (λ (xs f) (append-map f xs))))

;; Identity monad.
(define Id
  (monad
   (λ (x) x)
   (λ (xs f) (f xs))))

;; Maybe monad.
(struct just (elem) #:transparent)
(struct nothing () #:transparent) 

(define Maybe
  (monad
   ;; return : x -> Maybe x
   (λ (x) (just x))
   ;; bind : Maybe x -> (x -> Maybe y) -> Maybe y
   (λ (xs f)
     (if (just? xs)
         (f (just-elem xs))
         (nothing)))))


;; Continuation monad.
(define Cont
  (monad 
   ;; return : x -> (x -> r) -> r
   (λ (x) (λ (f) (f x)))
   ;; bind : ((x -> r) -> r) ->
   ;;        (x -> ((y -> r) -> r)) ->
   ;;        ((y -> r) -> r)
   (λ (d f) (λ (k) (d (λ (x) ((f x) k)))))
   ))
   
(provide (struct-out monad))
(provide List)
(provide (struct-out nothing))
(provide (struct-out just))
(provide Maybe)
(provide Id)
(provide Cont)
