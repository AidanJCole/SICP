#lang sicp
;Comments

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) 
         (square (expt b (/ n 2))))
        (else 
         (* b (expt b (- n 1))))))

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

;(define (pi-sum a b)
;  (define (pi-term x)
;    (/ 1.0 (* x (+ x 2))))
;  (define (pi-next x)
;    (+ x 4))
;  (sum pi-term a pi-next b))

;(define (integral f a b dx)
;  (define (add-dx x) (+ x dx))
;  (* (sum f (+ a (/ dx 2.0)) add-dx b)
;     dx ))

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
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

; ex 1.33 a
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

; ex 1.33 b
(define (gcd a b)
  (if (= a 0)
      b
      (if (= b 0)
          a
          (gcd b (remainder a b)))))

(define (coprime a b)
  (= 1 (gcd a b)))

(define (prod-rel-prime n)
  (define (rel a) (coprime a n))
  (filtered-accumulate rel * 1 identity 0 inc n))

; 1.3.2 Lambda

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; (lambda (<formal-paramaters>) <body>)

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance .00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Excercise 1.35

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1))

; Excercise 1.36

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; takes 35 guesses
;(define x-to-x-is-1000 (fixed-point-print (lambda (x) (/ (log 1000) (log x))) 2))

; takes 9 guesses
;(define x-to-x-is-1000-damped (fixed-point-print (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2))

; Excercise 1.37a

(define (cont-frac n d k)
  (define (cont-frac-iter n d k total)
    (if (= 0 k)
        total
        (cont-frac-iter n d (- k 1) (/ (n k) (+ total (d k))))))
  (cont-frac-iter n d k 0))

;(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 11)

(define (cont-frac-rec n d k)
  (define (recur i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ 1 i))))))
  (recur 1))

; Excercise 1.38

(define (e-approx k) (cont-frac (lambda (x) 1.0) (lambda (x) (if (= (remainder (+ x 1) 3) 0)
                                                                 (* 2 (/ (+ x 1.0) 3))
                                                                 1.0)) k))

; Excercise 1.39

(define (tan-cf x k) (cont-frac
                      (lambda (i) (if (= 1 i)
                                      x
                                      (- (* x x))))
                      (lambda (i) (- (* i 2) 1.0))
                      k))

; 1.3.4 Procedures as returned values

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (define dx .00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
;Excercise 1.40

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
(define (cubic-zero a b c) (newtons-method (cubic a b c) 1))

;Excercise 1.41

(define (double x) (lambda (y) (x (x y))))

;Excercise 1.42

(define (compose f g) (lambda (x) (f (g x))))

;Excercise 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;Excercise 1.44

(define (smooth f)
  (define dx .0001)
  (lambda (x ) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)))

(define (n-folded-smooth f n) (repeated (smooth f) n))

;Excercise 1.45

(define (nth-root x n)
  (fixed-point ((repeated average-damp (- n 1)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

;Excercise 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter-improve guess)
      (if (good-enough? guess)
          guess
          (iter-improve (improve guess))))
    (iter-improve guess)))

(define (sqrt4 x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess)))) 1.0))

(define (fixed-point2 f first-guess)
  (define tolerance .00001)
  ((iterative-improve (lambda (x) ((< (abs (- x (f x))) tolerance)))) first-guess))

; 2.1.1

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;Excercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display(x-point p))
  (display",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

;Excercise 2.3

(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))

(define (area-rect r)
  (* (width-rect r) (height-rect r)))

; Rectangle is orthogonal to the x/y axis
; p1 is the origin of the rectangle
(define (make-rect p1 width height)
  (cons p1 (cons width height)))

(define (width-rect r)
  (abs (car (cdr r))))

(define (height-rect r)
  (abs (cdr (cdr r))))

;Excercise 2.4

;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

;Excercise 2.5

;(define (cons x y)
;  (* (expt 2 x) (expt 3 y)))

;(define (car p)
;  (if (= (remainder p 2) 0)
;      (+ (car (/ p 2)) 1)
;      0))

;(define (cdr p)
;  (if (= (remainder p 3) 0)
;      (+ (cdr (/ p 3)) 1)
;      0))

;Excercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((sxl (positive? (lower-bound x)))
        (sxu (positive? (upper-bound x)))
        (syl (positive? (lower-bound y)))
        (syu (positive? (upper-bound y)))
        (lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (if (and sxl sxu syl syu)
        (make-interval (* lx ly) (* ux uy))
        (if (and (not sxl) sxu syl syu)
            (make-interval (* lx uy) (* ux uy))
            (if (and (not (or sxl sxu)) syl syu)
                (make-interval (* lx uy) (* ux ly))
                (if (and sxl sxu (not syl) syu)
                    (make-interval (* ux ly) (* ux uy))
                    (if (and (not ( or sxl sxu syl)) syu)
                        (make-interval (* lx uy) (* lx ly))
                        (if (and sxl sxu (not (or syl syu)))
                            (make-interval (* ux ly) (* lx uy))
                            (if (and sxu (not (or sxl syl syu)))
                                (make-interval (* ux ly) (* lx ly))
                                (if (not (or sxl sxu syl syu))
                                    (make-interval (* ux uy) (* lx ly))
                                    (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy)))))))))))))


(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Division error (interval spans 0)" y)
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

; Excercise 2.7

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

; Excercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (eql-interval? x y)
  (and (= (lower-bound x) (lower-bound y)) (= (upper-bound x) (upper-bound y))))

(define (test-mul lx1 lx2 ly1 ly2)
    (if (or (null? lx1) (null? lx2) (null? ly1) (null? ly2))
        nil
        (begin
         (if (not (eql-interval? (old-mul-interval (make-interval (car lx1) (car lx2)) (make-interval (car ly1) (car ly2))) (mul-interval (make-interval (car lx1) (car lx2)) (make-interval (car ly1) (car ly2)))))
             (begin
              (newline)
              (display (make-interval (car lx1) (car lx2)))
              (newline)
              (display (make-interval (car ly1) (car ly2)))
              (newline)
              (display (old-mul-interval (make-interval (car lx1) (car lx2)) (make-interval (car ly1) (car ly2))))
              (newline)
              (display (mul-interval (make-interval (car lx1) (car lx2)) (make-interval (car ly1) (car ly2)))))
             nil)
         (test-mul lx1 lx2 ly1 (cdr ly2))
         (test-mul lx1 lx2 (cdr ly1) (cdr ly2))
         (test-mul lx1 (cdr lx2) (cdr ly1) (cdr ly2))
         (test-mul (cdr lx1) (cdr lx2) (cdr ly1) (cdr ly2))
        )
        )
  )

; Excercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (* c (+ 1 (/ p 100))) (* c (- 1 (/ p 100)))))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

; Excercise 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; 2.2.1

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Excercise 2.17

(define (last-pair items)
  (list-ref items (- (length items) 1)))

; Excercise 2.18

(define (reverse items)
  (define (iter items r)
    (if (null? items)
        r
        (iter (cdr items) (cons (car items) r))))
  (iter items nil))

; Excercise 2.19

(define (count-us-change amount)
  (cc amount us-coins))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (first-denomination values) (car values))
  (define (except-first-denomination values) (cdr values))
  (define (no-more? values) (null? values))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

;Excercise 2.20

(define (same-parity s . r)
  (define (recur s r)
  (if (null? r)
      r
      (if (= (remainder s 2) (remainder (car r) 2))
          (cons (car r) (recur s (cdr r)))
          (recur s (cdr r)))))
  (recur s r))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;Excercise 2.23

(define (for-each f items)
  (if (null? items)
      #f
      (and (f (car items)) (for-each f (cdr items)))))

;Section 2.2.2

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;Ex 2.27

(define (deep-reverse x)
  (define (iter items r)
    (if (null? items)
        r
        (if (pair? (car items))
            (iter (cdr items) (cons (deep-reverse (car items)) r))
            (iter (cdr items) (cons (car items) r)))))
  (iter x nil))

;Ex 2.28

(define (fringe x)
  (define (iter x l)
    (cond ((pair? x) (append (iter (car x) l) (iter (cdr x) l)))
          ((null? x) l)
          (else (cons x l))))
  (iter x nil))

