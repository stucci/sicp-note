#lang racket

(display "\n### exercise1.1 ###\n")
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

(display "\n### exercise1.2 ###\n")
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(display "\n### exercise1.3 ###\n")
(define (square-plus a b)
    (+ (* a a) (* b b)))

(define (f a b c)
    (cond ((and (<= a b) (<= a c)) (square-plus b c))
          ((and (<= b c) (<= b a)) (square-plus c a))
          ((and (<= c a) (<= c b)) (square-plus a b))
    )
)

(f 1 3 4)

(display "\n### exercise1.4 ###\n")
(define (a-plus-abs-b a b)
    ((if(> b 0) + -) a b))
(a-plus-abs-b 3 (- 5))

(display "\n### exercise1.5 ###\n")
(define (p) (p))
(define (test x y)
    (if (= x 0) 0 y))
; (test 0 (p))

(display "\n### exercise1.6 ###\n")
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

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
    (else else-clause)))

(define (new-sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (new-sqrt-iter (improve guess x) x)))
(define (new-sqrt x)
    (new-sqrt-iter 1.0 x))
; (new-sqrt 9)
; (new-sqrt 2)

; (display "\n### exercise1.7 ###\n") skip

(display "\n### exercise1.8 ###\n")
(define (cube x) (* x x x))
(define (cbrt-iter guess x)
    (if (cugood-enough? guess x)
        guess
        (cbrt-iter (cuimprove guess x) x)))
(define (cuimprove guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cugood-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
(define (cbrt x)
    (cbrt-iter 1.0 x))
(cube 3)
(cuimprove 4.0 27.0)
(cugood-enough? 3.00001 27)
(cbrt 27)
