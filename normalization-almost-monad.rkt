#lang racket

(require leftdo/normalized-distributions)
(require leftdo/monad)


(define norm-return dist-return)

(define (norm-bind xs f)
  (dist-normalize (dist-bind xs f)))

(define (norm-map f xs)
  (dist-normalize (dist-map f xs)))

(define Norm
  (monad
    norm-return
    norm-bind
    norm-map))

(provide Norm)
(provide norm-return norm-bind norm-map)
