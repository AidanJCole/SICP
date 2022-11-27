#lang sicp
;Comments

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;ex 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes s e)
  (cond ((even? s) (search-for-primes (+ s 1) e))
        ((> s e))
        (else (timed-prime-test s) (search-for-primes (+ s 2) e))))

;1.3.1

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx ))

(define (simpson_rule f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (cond ((= k 0) (f (+ a (* k h))))
          ((= k n) (f (+ a (* k h))))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))))
  (/ (* h (sum term 0 inc n)) 3))

;excercise 1.31 a

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial a)
    (product identity 1 inc a))

(define (pi-over-four n)
  (define (frac-next n) (/ (+ 2 (* 2 (truncate (/ (+ n 1) 2)))) (+ 3 (* 2(truncate (/ n 2))))))
  (product frac-next 0 inc n))

; ex 1.31 b

(define (iter-product term a next b)
  (define (iter-prod total term a next b)
    (if (> a b)
        total
        (iter-prod (* total (term a)) term (next a) next b)))
  (iter-prod 1 term a next b))

; ex 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (acc-fac a)
  (accumulate * 1 identity 1 inc a))

; ex 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter (term a))
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b)
          (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))