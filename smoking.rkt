#lang racket

;; smoking.rkt
;;
;; This file contains an example of causality and intervention analysis.
;; It follows an example of the front-door criterion from the work of Pearl
;; Glymour, and Jewell (page 66). Numbers come from Table 3.1 there.
;;
;; Reference.
;; Causal Inference in Statistics: A Primer -- Pearl, Glymour, and Jewell.


(require leftdo/monad)
(require leftdo/subdistributions)
(require leftdo/normalization-almost-monad)
(require leftdo/left-do)

(define (observe x y)
  (if (equal? x y)
      (uniform (list '()))
      (uniform (list))))


(define survey
  (distribution
     [(list 'smoker 'tar 'nocancer)    323/800]
     [(list 'smoker 'tar 'cancer)       57/800]
     [(list 'nonsmoker 'tar 'nocancer)    1/800]
     [(list 'nonsmoker 'tar 'cancer)     19/800]
     [(list 'smoker 'notar 'nocancer)   18/800]
     [(list 'smoker 'notar 'cancer)      2/800]
     [(list 'nonsmoker 'notar 'nocancer) 38/800]
     [(list 'nonsmoker 'notar 'cancer)  342/800]))

(define (estimate-intervention i)
  (lDo Norm
    zp <- (lDo Norm
               (list x z y) <- survey
               '() <- (observe i x)
               return z)
    xp <- (lDo Norm
               (list x z y) <- survey
               return x)
    y  <- (lDo Norm
               (list x z y) <- survey
               '() <- (observe x xp)
               '() <- (observe z zp)
               return y)
    return y))



;; Dummy data.
(define dummydata
  (distribution
     [(list 'gene 'tar 'smoker 'nocancer)    323/800]
     [(list 'gene 'tar 'smoker 'cancer)       57/800]
     [(list 'gene 'tar 'nosmoker 'nocancer)    1/800]
     [(list 'gene 'tar 'nosmoker 'cancer)     19/800]
     [(list 'gene 'notar 'smoker 'nocancer)   18/800]
     [(list 'gene 'notar 'smoker 'cancer)      2/800]
     [(list 'gene 'notar 'nosmoker 'nocancer) 38/800]
     [(list 'gene 'notar 'nosmoker 'cancer)  342/800]
     [(list 'nogene 'tar 'smoker 'nocancer)    303/800]
     [(list 'nogene 'tar 'smoker 'cancer)       77/800]
     [(list 'nogene 'tar 'nosmoker 'nocancer)    9/800]
     [(list 'nogene 'tar 'nosmoker 'cancer)     11/800]
     [(list 'nogene 'notar 'smoker 'nocancer)   13/800]
     [(list 'nogene 'notar 'smoker 'cancer)      7/800]
     [(list 'nogene 'notar 'nosmoker 'nocancer) 78/800]
     [(list 'nogene 'notar 'nosmoker 'cancer)  302/800]))

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




;; Dummy data version.
;; This is P(-), our main measure.
(define model
  (lDo Norm
       u <- prevalence
       x <- (smokes u)
       z <- (tar x)
       y <- (health u z)
       return (list u x z y)))

;; This is P*(-), our intervened measure.
(define smoke-intervention
  (lDo Norm
       u <- prevalence
       x <- (lDo Norm
                 x <- (smokes u)
                 '() <- (observe x 'smoker)
                 return x)
       z <- (tar x)
       y <- (health u z)
       return (list u x z y)))

;; Let us check that P*(u) = P(u).
(define eq1-lhs
  (lDo Norm
       (list u x z y) <- smoke-intervention
       return u))
(define eq1-rhs
  (lDo Norm
       (list u x z y) <- model
       return u))


;; Let us check that P*(y|x,u) = P(y|x,u).
;; Note that these are not equal as channels, but only equal on the
;; already given intervention.
(define (eq2-lhs iu)
  (lDo Norm
       (list u x z y) <- smoke-intervention
       ;'() <- (observe x 'smoker)
       '() <- (observe u iu)
       return y))
(define (eq2-rhs iu)
  (lDo Norm
       (list u x z y) <- model
       '() <- (observe x 'smoker)
       '() <- (observe u iu)
       return y))

;; Let us check that P*(y|x) = SUM{u} P(y|x,u) P(u).
(define eq3-lhs
  (lDo Norm
       (list u x z y) <- smoke-intervention
       '() <- (observe x 'smoker)
       return y))

(define eq3-rhs
  (lDo Norm
       (list u xa za ya) <- model
       y <- (lDo Norm
                 (list ub x z y) <- model
                 '() <- (observe x 'smoker)
                 '() <- (observe u ub)
                 return y)
       return y))
;; This is the front-door criterion!


;;;;;
;; BACK-DOOR
;;;;;

;; First, no confounding, P(z|do(x)) = P*(z|x) = P(z|x).
(define eq4-lhs
  (lDo Norm
       (list u x z y) <- model
       '() <- (observe x 'smoker)
       return z))
(define eq4-rhs
  (lDo Norm
       (list u x z y) <- smoke-intervention
        '() <- (observe x 'smoker)
       return z))

;; P(y|do(z)) = Q(y|z) = SUM{x'} P(y|x',z)P(x')
(define (tar-intervention iz)
  (lDo Norm
       u <- prevalence
       x <- (smokes u)
       z <- (lDo Norm
                 z <- (tar x)
                 '() <- (observe z iz)
                 return z)
       y <- (health u z)
       return (list u x z y)))  

(define (eq5-lhs iz)
  (lDo Norm
       (list u x z y) <- (tar-intervention iz)
       return y))

(define (eq5-rhs iz)
  (lDo Norm
       (list ua xp za ya) <- model
       y <- (lDo Norm
                 (list u x z y) <- model
                 '() <- (observe x xp)
                 '() <- (observe z iz)
                 return y)
       return y))

;; Can we jump to the last equation?
;; P*(y|do(x)) = SUM{z} P(z|x) SUM{x'} P(y|x',z) P(x')
(define eq6-lhs
  (lDo Norm
       (list u x z y) <- smoke-intervention
       return y))
(define eq6-rhs
  (lDo Norm
       zp <- (lDo Norm
                 (list u x z y) <- model
                 '() <- (observe x 'smoker)
                 return z)
       xp <- (lDo Norm
                 (list u x z y) <- model
                 return x)
       y <- (lDo Norm
                 (list u x z y) <- model
                 '() <- (observe x xp)
                 '() <- (observe z zp)
                 return y)
       return y))

;; This is backdoor!




(define smoke-marginal
  (lDo Norm
       (list u x z y) <- smoke-intervention
       return (list y)))

(define recover-intervention
  (lDo Norm
       (list u xa za ya) <- model
       (list ub x z y) <- model
       '() <- (observe u ub)       
       '() <- (observe x 'smoker)
       return (list y)))

(define test
  (lDo Norm
       (list u xa za ya) <- model
       (list ub x z y) <- model
       '() <- (observe u 'gene)
       return (list y)))

