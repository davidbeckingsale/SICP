; Exercise 1.1

10
; Value: 10

(+ 5 3 4)
; Value: 12

(- 9 1)
; Value: 8

(/ 6 2)
; Value: 3

(+ (* 2 4) (- 4 6))
; Value: -16

(define a 3)
; Value; a

(define b (+ a 1))
; Value: b

(+ a b (* a b))
; Value: 19

(= a b)
; Value: #f

(if (and (> b a) (< b (* a b)))
     b
     a)
; Value: 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; Value: 16

(+ 2 (if (> b a) b a))
; Value: 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; Value: 16

; Exercise 1.2
(/ (+ 5 4
      (- 2 
         (- 3 
            (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

; Using the 3-way conditional to express the 3 positions that the lowest number
; might take in the argument list: first, second, or third.
(define (sum-of-largest-squares a b c)
  (cond ((and (> a c) (> b c)) (sum-of-squares a b))
        ((and (> a b) (> c b)) (sum-of-squares a c))
        ((and (> b a) (> c a)) (sum-of-squares b c))))

; Exercise 1.4
;
; The if-test is used to determine which operator to use in order to add the
; absolute value of b to a.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.5
;
; normal order -> "fully expand, then reduce"
; This means that all the procedures are expanded, but there arguments are not
; evaluated until required.
;
; applicative order -> "evaluate the arguments, then apply"
; Does what it says, arguments will be evaluated, then procedure applied.
;
; (define (p) (p))
; (define (test x y)
;  (if (= x 0) 0 y))

; (test 0 (p))
;
; normal order: (test 0 (p)) -> (if (= 0 0) 0 (p)) -> (if (= 0 0) 0 (p)) -> (if (#t) 0 (p)) -> 0
; (p) is never evaluated.
;
; applicative order -> (test 0 (p)) -> cannot evaluate p!

; 1.1.7 Example: Square Roots by Newton's Method
;
; Function: declarative (what is).
; Procedure: imperative (how to).
;
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess previous)
  (< (abs (- previous guess)) (* 0.00000001 guess)))

; Exercise 1.6 
;
; Using new-if hits the maximum stack depth, since it's evaluating the
; arguments in applicative order, and the els eclause will never terminate.
(define (new-if pred then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess previous x)
  (if (good-enough? guess previous)
    guess
    (sqrt-iter (improve guess x) guess x)))

; (define (sqrt x)
;   (sqrt-iter 1.0 0.0 x))

; Exercise 1.7
;
; (sqrt 0.001) => 0.0412454, and should be 0.031622777
; 
; Update good-enough? to consider difference between guess and previous guess.

; Exercise 1.8
(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt-iter guess previous x)
  (if (good-enough? guess previous)
    guess
    (cbrt-iter (improve-cbrt guess x) guess x)))

(define (cbrt x)
  (cbrt-iter 1.0 0.0 x))

; Parameters are local to the body of a procedure.
; Procedures can be defined internal to other procedures.

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; In this new version of sqrt, x is a free variable in the procedures
; good-enough? and improve.


