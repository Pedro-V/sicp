;; A programmer is like a wizard:
;; He/she conjures spells (programs) in an appropriate language (Lisp) for
;; guiding the spirit that lives in the computer (processes)

;; The elements of programming are:
;; - Primitives
;; - Means of combinations
;; - Means of abstraction
;; from primitives, we compose combinations
;; from combinations, we abstract them away
;; we treat these new abstractions as primitive

;; Some primitives: Data and procedures aren't so different
10
display
+

;; A combination
(+ (/ 3. 2)
   (- 3/2 -1001.29))

;; A first form of abstraction: define
(define (name) "Lisp")

(define sum +)

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (is-leap-year year)
  (if (zero? (/ year 4))
      (if (zero? (/ year 100))
          (if (zero? (/ year 400))
              #t))
      #f))


;; Exercise 1.2

(define expression
  (/ (+ 5 4 2 -3 -6 -4/5)
     (* 3
        (- 6 2)
        (- 2 7))))

;; Exercise 1.3

(define (greatest-two a b c)
  (let ((max1 (max a (max b c))))
    (let ((max2 (if (= max1 a)
                    (max b c)
                    (if (= max1 b) c a))))
      (list max1 max2))))

(define (sum-of-larger-squares x y z)
  (let ((max-two (greatest-two x y z)))
    (sum-of-squares (car max-two)
                    (cadr max-two))))
;; Exercise 1.4
;; if 'b' is positive, the procedure to be used as operator is +, else it is -

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; Exercise 1.5
;; Applicative-order: It will fall in infinite recursion, since the second argument
;; will have to be evaluated
;;
;; Normal-order: It will output 0, since the second parameter won't be avaliated,
;; since it's never needed.


;; Square-root approximation using Newton's method

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (<= (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average  guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter (/ x 2) x))
