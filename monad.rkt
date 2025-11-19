#lang racket


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
(struct just (elem))
(struct nothing ()) 

(define Maybe
  (monad
   ;; return : x -> Maybe x
   (λ (x) (just x))
   ;; bind : Maybe x -> (x -> Maybe y) -> Maybe y
   (λ (xs f)
     (if (just? xs)
         (f (just-elem xs))
         (nothing)))))


(provide (struct-out monad))
(provide (struct-out nothing))
(provide (struct-out just))
(provide List Id Maybe)