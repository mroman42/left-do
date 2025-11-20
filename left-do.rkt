#lang racket

(require leftdo/monad)


;; Do-notation
(define-syntax rdo
  (syntax-rules (<- return)
    [(rdo m var <- mexp rest ...)
     ((monad-bind m) mexp (match-lambda [var (rdo m rest ...)]))]
    [(rdo m return value)
     ((monad-return m) value)]))

(define-syntax doLeftAcc
  (syntax-rules (<- return)
    [(doLeftAcc m acc accVar
                var <- mexp
                rest ...)
     (doLeftAcc m
                (rdo m
                  accVar <- acc
                  var <- mexp
                  return (list var accVar))
                (list var accVar)
                rest ...)]
    [(doLeftAcc m acc accVar
                return var)
     (rdo m
       accVar <- acc
       return var)]))

(define-syntax ldo
  (syntax-rules (<- return)
    [(ldo m rest ...)  (doLeftAcc m (rdo m return (list)) (list) rest ...)]))


(provide rdo ldo)