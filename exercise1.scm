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
; (define (p) (p))
; (define (test x y)
;     (if (= x 0) 0 y))
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

(display "\n### exercise1.7 skip ###\n")

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

(display "\n### exercise1.9 ###\n")
; (define (+ a b)
    ; (if (= a 0) b (inc (+ (dec a) b))))
(display "(inc (inc (inc (inc (+ 0 5)))))\n")
(display "Therfore recursive process\n")
; (define (+ a b)
;     (if (= a 0) b (+ (dec a) (inc b))))
(display "(+ 3 6) -> (+ 2 7) -> (+ 1 8) -> (+ 0 9) => 9\n")
(display "Therfore iterative process\n")

(display "\n### exercise1.10 ###\n")
(define (A x y)
    (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(A 0 1)
(A 0 2)
(A 0 3)
(A 0 4)
(A 0 5)
(display "(A 0 n) => 2*n (trivial)\n")
(A 1 1)
(A 1 2)
(A 1 3)
(A 1 4)
(A 1 5)
(display "(A 1 n) => 2^n\n")
(A 2 1)
(A 2 2)
(A 2 3)
(A 2 4)
; (A 2 5) => 2^65536 too big...
(display "(A 2 n) => 2↑↑n\n")

(display "\n### exercise1.11 ###\n")
(define (f-rec n)
    (cond ((< n 3) n)
        (else (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))))
    )
)
(f-rec 1)
(f-rec 2)
(f-rec 3)
(f-rec 4)
(f-rec 5)
(f-rec 6)

(define (f-itr n)
    (cond ((< n 3) n)
        (else (f-itr-itr 2 1 0 n))))
(define (f-itr-itr a b c cnt)
    (if (= cnt 2)
        a
        (f-itr-itr (+ a (* 2 b) (* 3 c)) a b (- cnt 1))
    )
)
(f-itr 1)
(f-itr 2)
(f-itr 3)
(f-itr 4)
(f-itr 5)
(f-itr 6)

(display "\n### exercise1.12 ###\n")
(define (pascal x y)
    (cond ((or (= x 1) (= y 1) (= x y)) 1)
        (else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))
    )
)
(pascal 1 1)
(pascal 1 2) (pascal 2 2)
(pascal 1 3) (pascal 2 3) (pascal 3 3)
(pascal 1 4) (pascal 2 4) (pascal 3 4) (pascal 4 4)
(pascal 1 5) (pascal 2 5) (pascal 3 5) (pascal 4 5) (pascal 5 5)

(display "\n### exercise1.13 skip ###\n")

(display "\n### exercise1.14 skip ###\n")

(display "\n### exercise1.15 ###\n")
(define cnt 0)
(define (p x)
    (set! cnt (+ cnt 1))
    (- (* 3 x) (* 4 (cube x)))
)
(define (sine angle c)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0) (+ c 1)))
    )
)

(sine 12.15 0)
cnt

(set! cnt 0)
(sine 1 0)
cnt

(set! cnt 0)
(sine 2 0)
cnt

(set! cnt 0)
(sine 3 0)
cnt

(set! cnt 0)
(sine 8 0)
cnt

(set! cnt 0)
(sine 9 0)
cnt

(set! cnt 0)
(sine 25 0)
cnt

(display "space O(1), step O(log_3 a)")
