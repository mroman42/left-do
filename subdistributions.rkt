#lang br

(require racket/struct)
(require rackunit)
(require leftdo/monad)

(struct subdistribution (results))

(provide
 dist-bind
 dist-return
 (rename-out [dist-uniform uniform])
 dist-normalize
 dist-void)


(define (pair x y) (list x y))

(define example
  (list
   (pair 'a #e0.3)
   (pair 'b #e0.7)
   (pair 'c #e0.0)))

(define example-2
  (list
   (pair 3 0.3)
   (pair 4 0.7)))



;; Validity.
(define (validity xs)
  (apply + (map second xs)))

;; Subdistributions form a functor.
(define/match (dist-map f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list (f x) v) (dist-map f ys))])

;; But we can also map on the values.
(define/match (dist-map-values f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list x (f v)) (dist-map-values f ys))])

;; Remove zeroes
(define/match (remove-zeroes xs)
    [('())  null]
    [((cons (list x 0) ys))  (remove-zeroes ys)]
    [((cons (list x v) ys))  (cons (list x v) (remove-zeroes ys))])

(check-equal?
 (remove-zeroes
  (list
   (pair 'a #e0.3)
   (pair 'b #e0.7)
   (pair 'c #e0.0)))
 (list
  (list 'a #e0.3)
  (list 'b #e0.7)))

;; Weight of a single point of a distribution.
(define/match (weight-of-point x xs)
  [(x '()) 0]
  [(x (cons (list x v) ys)) (+ v (weight-of-point x ys))]
  [(x (cons (list y v) ys)) (weight-of-point x ys)])

(check-equal? (weight-of-point 'a example) #e0.3)


;; Reweighting a distribution.
(define/match (dist-remove x xs)
  [(x '())  '()]
  [(x (cons (list x v) xs))  (dist-remove x xs)]
  [(x (cons (list y v) xs))  (cons (list y v) (dist-remove x xs))])

(define/match (reweight xs)
  [('()) '()]
  [((cons (list x v) ys))
   (let ([w   (+ v (weight-of-point x ys))])
     (cons (list x w) (reweight (dist-remove x ys))))])
     

;; Condensing a distribution into a valid distribution.
(define (condense xs)
  (reweight (remove-zeroes xs)))

(define example-3
  (list
   (list 'a #e0.3)
   (list 'b #e0.1)
   (list 'b #e0.1)
   (list 'a #e0.1)
   (list 'c #e0.0)))

(check-equal?
 (condense example-3)
 (list
  (list 'a #e0.4)
  (list 'b #e0.2)))

;; Subdistributions of subdistributions.
(define/match (rescale xss)
  [((list xs v))  (dist-map-values (lambda (x) (* v x)) xs)])

(define (dist-join xss)
  (condense (apply append (map rescale xss))))

(define (dist-normalize xs)
  (condense
   (dist-map-values (lambda (v) (/ v (validity xs))) xs)))

(define example-4
  (list
   (pair
    (list
     (pair 'a #e0.3)
     (pair 'b #e0.7))
    #e0.4)
   (pair
    (list
     (pair 'b #e0.2)
     (pair 'a #e0.3)
     (pair 'c #e0.5))
    #e0.6)))
  
;; Monadic bind and return.
(define (dist-bind xs f)
  (dist-join (dist-map f xs)))

(define (dist-return x)
  (list (pair x #e1)))


;; Some basic examples.
(define (dist-uniform ls)
  (map (lambda (x) (pair x (/ #e1 (length ls)))) ls))

(define (dist-coin p)
  (list
    (pair #t p)
    (pair #f (- 1 p))))

(define dist-void
  (list))


(define Subd
  (monad
    dist-return
    dist-bind))

(define-syntax distribution
  (syntax-rules ()
    [(_ [x v] rest ...)  (cons (pair x v) (distribution rest ...))]
    [(_)                 (list)]))

(provide
 dist-bind dist-return
 (rename-out [dist-uniform uniform])
 dist-void)
(provide Subd)
(provide distribution)
