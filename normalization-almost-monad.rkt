#lang racket

(require leftdo/normalized-distributions)
(require leftdo/monad)


(define norm-return dist-return)

(define (norm-bind xs f)
  (dist-normalize (dist-bind xs f)))

(define Norm
  (monad
    norm-return
    norm-bind))

(provide Norm)
(provide norm-return)
(provide norm-bind)