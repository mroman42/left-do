#lang racket

(require leftdo/monad)


;; Do-notation
(define-syntax rdo
  (syntax-rules (<- pure)
    [(rdo m var <- mexp rest ...)
     ((monad-bind m) mexp (match-lambda [var (rdo m rest ...)]))]
    [(rdo m pure value)
     ((monad-return m) value)]))

(define-syntax doLeftAcc
  (syntax-rules (<- pure)
    [(doLeftAcc m acc accVar
                var <- mexp
                rest ...)
     (doLeftAcc m
                (rdo m
                  accVar <- acc
                  var <- mexp
                  pure (list var accVar))
                (list var accVar)
                rest ...)]
    [(doLeftAcc m acc accVar
                pure var)
     (rdo m
       accVar <- acc
       pure var)]))

(define-syntax ldo
  (syntax-rules (<- pure)
    [(ldo m rest ...)  (doLeftAcc m (rdo m pure (list)) (list) rest ...)]))


(provide rdo ldo)