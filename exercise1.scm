#lang racket

; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a)(< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1))

; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; 1.3
(define (square-plus a b)
    (+ (* a a) (* b b)))

(define (f a b c)
    (cond ((and (<= a b) (<= a c)) (square-plus b c))
          ((and (<= b c) (<= b a)) (square-plus c a))
          ((and (<= c a) (<= c b)) (square-plus a b))
    )
)

(f 1 3 4)

; 1.4
(define (a-plus-abs-b a b)
    ((if(> b 0) + -) a b))
(a-plus-abs-b 3 (- 5))

; 1.5
(define (p) (p))
(define (test x y)
    (if (= x 0) 0 y))
; (test 0 (p))

; 1.6
(define (square x) (* x x))
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
(define (improve guess x)
    (average guess (/ x guess)))
(define (average x y)
    (/ (+ x y) 2))
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
    (sqrt-iter 1.0 x))
(sqrt 9)
(sqrt 2)