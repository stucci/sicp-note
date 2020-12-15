#lang racket

(define (square x) (* x x))
(define (cube x) (* x x x))

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

(display "space O(1), step O(log_3 a)\n")

(display "\n### exercise1.16 ###\n")
(define (expt b n)
    (expt-iter b n 1))
(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product))
    )
)
(expt 2 5)

(define (even? n)
    (= (remainder n 2) 0))
(define (fast-expt b n)
    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
    )
)
(fast-expt 3 3)

(define (fast-expt-iter b n)
    (fast-expt-iter-iter b n 1)
)
(define (fast-expt-iter-iter b n a)
    (cond ((= n 1) a)
          ((even? n) (fast-expt-iter-iter b (/ n 2) (* b b a)))
          (else (fast-expt-iter-iter b (- n 1) (* b a)))
    )
)
(fast-expt-iter 2 3)
(fast-expt-iter 2 4)
(fast-expt-iter 2 5)
(fast-expt-iter 3 3)
(fast-expt-iter 3 4)
(fast-expt-iter 3 5)

(display "\n### exercise1.17 ###\n")
(define (*-rec a b)
    (if (= b 0)
        0
        (+ a (*-rec a (- b 1)))
    )
)
(*-rec 2 3)
(*-rec 5 8)

(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (fast-*-rec a b)
    (cond ((= b 1) a)
          ((even? b) (+ (double a) (fast-*-rec (double a) (halve (- b 2)))))
          (else (+ a (fast-*-rec (double a) (halve (- b 1)))))
    )
)
(fast-*-rec 5 1)
(fast-*-rec 5 7)
(fast-*-rec 5 8)
(fast-*-rec 7 9)

(display "\n### exercise1.18 ###\n")

(display "\n### exercise1.19 skip ###\n")
