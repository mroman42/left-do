#lang racket

(require leftdo/monad)


;; Do-notation
(define-syntax rDo
  (syntax-rules (<- return)
    [(rDo m var <- mexp rest ...)
     ((monad-bind m) mexp (match-lambda [var (rDo m rest ...)]))]
    [(rDo m return value)
     ((monad-return m) value)]))

(define-syntax accDo
  (syntax-rules (<- return)
    [(accDo m acc accVar
                var <- mexp
                rest ...)
     (accDo m
                (rDo m
                  accVar <- acc
                  var <- mexp
                  return (list var accVar))
                (list var accVar)
                rest ...)]
    [(accDo m acc accVar
                return var)
     (rDo m
       accVar <- acc
       return var)]))

(define-syntax lDo
  (syntax-rules (<- return)
    [(lDo m rest ...)
     (accDo m (rDo m return (list)) (list) rest ...)]))


(provide rDo lDo)