;G1 - TD1 - 26/01/2024

;Exercise 2
;-----------
(define word  "abracadabra")
; string abra
(substring word 0 4)
(substring word 7 11)

; string ra
(substring word 2 4)
(substring word 9 11)

; string r
(substring word 2 3)

; caracter b
(string-ref word 1)


;Exercise 3
;-----------
(define ch "Hello Word!")
; Test if the word is douled
(string=?
 (substring ch 0 (quotient (string-length ch) 2))
 (substring ch (quotient (string-length ch) 2) (string-length ch)))

; create a doubled word
(string-append ch ch)

;Exercise 4
; Square
(define (areasquare x y)
  (and (and (< x 5) (> x -5))   ; target all the x between -5 and 5
       (and (< y 5) (> y -5))))   ; target all the y between 5 and -5

; Circle
(define (arecircle x y)
  (< (+ (* x x) (* y y)) 25))

; Semi-plane
(define (semiplane x y)
    (< y x))

;Octogone
(define (octogone x y)
  (and (and (> x -6) (< x 6))  ; horizontal sides
       (and (> y -6) (< y 6))  ; vertical sides
       (and (and (< y (+ x 9)) (> y (- x 9)))   ; diagonal side with slope 1
            (and (< y (+ (- x) 9)) (> y (- (- x) 9))))))  ; diagonal side with slope -1

; vertical lines
(define (vertical x)
  (and (< (remainder (- x 1) 4) 1)
       (> x 0)))

;Rectangle triangle
(define (recttriangle x y)
  (and (and (> x -5) (> y -3))    ; vertical and horizontal lines
       (< (* 13 y) (- 33 (* x 9)))))   ; diagonal line


; Exercise 5
(define (exercise5 x1 x2 x3 x4 y1 y2 y3 y4)
  (not (or (or (> x1 x4) (> x3 x2))
      (or (> y2 y3) (> y4 y1)))))
