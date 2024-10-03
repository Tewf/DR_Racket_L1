;TD4 - 08/03/2024

;Exercise 1
(define (sumFirstIntegers n)
  (if (= n 0) 0
      (+ n (sumFirstIntegers (- n 1)))))

(define (exercise1)
  (display "Enter an integer greater or equal than 0 : ")
  (define n (read))
  (display "The sum of the fist integers of ") (display n) (display " is  ")
  (display (sumFirstIntegers n)))

;(exercise1)

; Exercise 2
(define (sumFirstEvenNumbers n)
  (cond ((= n 0) 0)
        ((= (remainder n 2) 0) (+ n (sumFirstEvenNumbers (- n 2))))
        (else (sumFirstEvenNumbers (- n 1)))))

(define (exercise2)
  (display "Enter an integer greater than 0 : ")
  (define n (read))
  (display "The sum of the fist even integers of ") (display n) (display " is  ")
  (display (sumFirstEvenNumbers n)))

;(exercise2)

; Exercise 3
(define (pow x n)
  ; returns the result of x power n
  ; real, integer >= 0 -> real
  (cond ((= n 0) 1)
        ((= x 0) 0)
        (else (* x (pow x (- n 1))))))

;(pow 5 2)

(define (globalPow x n)
  ; returns x elevated to the nth power
  ; real, integer -> real
  (if (>= n 0) (pow x n)
      (/ 1 (pow x (- n)))))


(define (approximationSquareRoot x n)
  ; returns an approximation to the nearest 1 by lower value of the nth square root of x,
  ; real >= 0, integer >= 0 -> real >= 0
  (approxSquareRoot x n 1))

(define (approxSquareRoot x n y)
  ; returns an approximation to the nearest 1 by lower value of the nth square root of x,
  ; it must be equal to MAX +1 of the the result
  ; real >= 0, integer >= 0, integer => 1 -> real >= 0
  (if (> (globalPow y n) x ) (- y 1)
         (approxSquareRoot x n (+ y 1))))
  

;(pow 3.2 3)
;(globalPow 2 -4)
;(approximationSquareRoot 2 1)
;(pow 0 2)
;(approximationSquareRoot 32.768 3)


;Exercise 4
(define (bcd a b)
  ; returns the biggest common divider between a and b
  ; 2 integers -> integer
  (if (< a b) 
      (bcdRecursion a b a)
      (bcdRecursion a b b)))

(define (bcdRecursion a b c)
  ; returns the biggest common divider between a and b, c helps with the recursion
  ; 3 integers -> integer 
  (if (and (= (remainder a c) 0)
           (= (remainder b c) 0))
      c
      (bcdRecursion a b (- c 1))))

;(bcd 1 2)
;(bcd 7 1)
;(bcd 500 150)


;Exercise 5
(define (fibonacci n)
  ; program to calculate the fibonacci of n
  ; integer -> integer
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;(fibonacci 10)
;(fibonacci 25)

; Exercise 6
(define (u n)
  ; Sequence u
  ; integer -> real
  (if (= n 0) 1
      (+ (* 3 (v (- n 1))) 4)))

(define (v n)
  ; Sequence v
  ; integer -> real
  (if (= n 0) 0
      (/ 1 (u (- n 1)))))

;(u 3)
;(v 8)
