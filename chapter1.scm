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

; 1.2.1 Linear Recursion and Iteration
;
; It's interesting how they introduce recursion without a second thought, I
; guess this is why using scheme as a teaching language is so powerful.

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; Writing an iterative factorial procedure:

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; Recursive process vs. recursive procedure
;
; A recursive procedure is one where the procedure refers to the procedure
; itself (a purely syntactic definition).  A process described as recursive
; follows a particular pattern of evolution, regardless of how the procedure is
; written.
;
; `fact-iter` is a recursive procedure, however it describes an iterative process.
;
; Scheme programs can express iterative processes with recursive procedures
; because only the state variables need to be stored, rather than the whole stack.

; Exercise 1.9
; 
; (define (+ a b)
; (if (= a 0) b (inc (+ (dec a) b))))
;
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; ...
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; ...
; (inc 8)
; 9
;
; Procedure is recursive.
;
; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))
;
; (+ 4 5)
; (+ (dec 4) (inc 5))
; ...
;
; Procedure is iterative.

; Exercise 1.10
;
; Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; (A 1 10)
; Value: 1024

; (A 2 4)
; Value: 65536

; (A 3 3)
; Value: 65536

; (define (f n) (A 0 n))
; (f n) computes 2n

(define (g n) (A 1 n))
; (g n) => (A 1 n) => (A 0 (A 1 (- y 1))) => ...
; (g n) computes 2^n

(define (h n) (A 2 n))
; (h n) => (A 2 n) => (A 1 (A 2 (- y 1))) => (A 0 (A x (- (A 2 (- y 1)) 1))) => 
; (h n) computes 2^h(n-1), where h(0) = 0 and h(1) = 2 (help from
; http://www.billthelizard.com/2009/11/sicp-exercises-19-and-110.html)
;
; Can also be expressed in terms of g: h(n) = g(2) applied n-1 times.

; 1.2.2 Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; Tree recursion trades time for space, as only the depth of the tree is
; stored, but number of steps in proportional to number of nodes.

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))


(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins)
                          kinds-of-coins))))))
  (cc amount 5))

; Exercise 1.11
; 
; f(n) = n, for n < 3
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3), for n >= 3

(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))
