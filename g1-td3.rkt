;TD3 - 16-02-2024

;Get the user's input as a String
;(symbol->string (read))

; Get a character from the user

;; converting a character to a symbol:
(define (char->symbol c)
  (string->symbol (string c)))

;(char->symbol #\a) ;;=> produces 'a

(define (symbol->char sym)
  (match (string->list (symbol->string sym))
    [(list ch) ch]
    [other (error 'symbol->char 
                  "expected a one-character symbol, got: ~s" sym)])) 

;(symbol->char 'a) ;; => produces #\a

; Exercise 1
; a
(define (isVowel? c)
  ;returns true if c is a vowel
  ; character -> boolean
  (or (char-ci=? c #\a)
      (char-ci=? c #\e)
      (char-ci=? c #\i)
      (char-ci=? c #\o)
      (char-ci=? c #\u)
      (char-ci=? c #\y)))

; b
(define (firstGrooup? verb)
  ; returns true if the verb is from the first group (ends with -er)
  ; string -> boolean
  (let ((wordLength (string-length verb)))
    (string=? "er" (substring verb (- wordLength 2 ) wordLength))))

;(display (firstGrooup? "manger"))

; c
(define (conjugaison verb)
  ; display the conjugaison of the given verb
  ; the verb must be of first group and ends wih -er
  ; string -> multiple line display
  (if (firstGrooup? verb)
      (let ((root (substring verb 0 (- (string-length verb) 2))))
        (begin
          (display (if (isVowel? (string-ref verb 0))
                       "j'"
                       "je "))
                   (display (string-append root "e"))(newline)
          (display "tu ")(display (string-append root "es"))(newline)
          (display "il/elle/on ")(display (string-append root "e"))(newline)
          (display "nous ")(display (string-append root "ons"))(newline)
          (display "vous ")(display (string-append root "ez"))(newline)
          (display "ils/elles ")(display (string-append root "ent"))(newline)))
      (begin
        (display "The given verb is not from the first group")(newline))))

;(display "Write a first group verb")
;(conjugaison (symbol->string (read)))


(define (exercise1d)
  ; program that gets a noun from the user, and its genre, and will display the nominal groupe
  ; no parameter -> display strings
  (display "Genre ? (m or f) ")
  (define genre (symbol->string (read)))
  (display "Noun ? ")
  (define noun (symbol->string (read)))
  (display (if (isVowel? (string-ref noun 0))
               "l'"
               (cond (( equal? genre  "m") "le ")
                     ((equal? genre "f") "la "))))
  (display noun)
  (newline))

;(exercise1d)

; Exercise 2
(define (max2Numbers a b)
  ; returns the greater number between a and b
  ; 2 real numbers -> 1 real number
  (if (> a b)
      a
      b))

(define (max3Numbers a b c)
  ; returns the greatest number between a, b and c
  ; 3 real numbers -> 1 real number
  (max2Numbers (max2Numbers a b) c))

(define (min2Numbers a b)
  ; returns the smaller number between a and b
  ; 2 real numbers -> 1 real number
  (if (> a b)
      b
      a))

(define (min3Numbers a b c)
  ; returns the smallest number between a, b and c
  ; 3 real numbers -> 1 real number
  (min2Numbers (min2Numbers a b) c))
 
(define (average a b)
  ; returns the average between a and b
  ; 2 real numbers -> 1 real number
  (/ (+ a b) 2))

(define (average3 a b c)
  ; returns the average between a, b and c
  ; 3 real numbers -> 1 real number
  (/ (+ a b c) 3))

(define (finalMark a b c)
  ; returns the maximum between the average of the 3 marks and between the average
  ; of the greatest and smallest mark
  ; 3 real numbers -> 1 real number
  (define average3Marks (average3 a b c))
  (define greatestMark (max3Numbers a b c))
  (define smallestMark (min3Numbers a b c))
  (define averageGreatestSmallestMark (average greatestMark smallestMark))
  (display "the final mark is ")
  (max2Numbers average3Marks averageGreatestSmallestMark))

;(finalMark 6 10 15)

;Exercise 3
(define (delta a b c)
  ; returns the discriminant of an equation ax²+ bx + c = 0
  ; real <> 0, 2 reals -> real
  (- (* b b) (* 4 a c)))

(define (solution a b c delta negative)
  ; returns the solution 1 of an equation ax² + bx + c = 0
  ; real <> 0, 2 reals, real >=0, boolean -> real
  (/ (+ (- b) (* (sqrt delta) (if negative -1 1))) (* 2 a)))

(define (exercise3)
  ;Programme that checks if 2 equations ax²+ bx + c = 0 have a common solution
  ; no entry -> display
  ; a1, a2: real <> 0
  ; b1, b2, c1, c2 : real
  ; delta1, delta2: real, those are the discriminants
  (display "Write a, b, c for the first equation")
  (define a1 (read)) (define b1 (read)) (define c1 (read))
  (display "Write a, b, c for the second equation")
  (define a2 (read)) (define b2 (read)) (define c2 (read))
  (define delta1 (delta a1 b1 c1))
  (define delta2 (delta a2 b2 c2))
  (cond ((or (< delta1 0) (< delta2 0)) (display "there is no common solution"))
        ((or (= (solution a1 b1 c1 delta1 #f) (solution a2 b2 c2 delta2 #f))
             (= (solution a1 b1 c1 delta1 #t) (solution a2 b2 c2 delta2 #f))
             (= (solution a1 b1 c1 delta1 #f) (solution a2 b2 c2 delta2 #t))
             (= (solution a1 b1 c1 delta1 #t) (solution a2 b2 c2 delta2 #t)))
         (display "There is at least one common solution"))
        (else (display "No common solution"))))

;(exercise3)


;Exercise 4
(define (exercise4)
  ; Program that emulates a calculator
  ; 2 reals from the user and an operation -> real
  (display "Give the operation to calculate (example: a+b) : ")
  (define inputUser (read))
  (define inputString (symbol->string inputUser))
  (define a (string-ref inputString 0))
  (define op (string-ref inputString 1))
  (define b (string-ref inputString 2))
  (display "Result : ")
  (display (calculator a op b)))

(define (calculator a op b)
  ; returns the result based on the given numbers and the operation
  ; character in {#\+, #\-, #\*, #\/}m 2 integres between 0 amd 9-> real
  (cond ((char=? op #\+) (+ (number a) (number b)))
        ((char=? op #\-) (- (number a) (number b)))
        ((char=? op #\*) (* (number a) (number b)))
        ((char=? op #\:) (/ (number a) (number b)))))

(define (number n)
  ; returns the number based on the given string
  ; char between #\0 and #\9 -> number between 0 and 9
  (cond ((char=? n #\0) 0)
        ((char=? n #\1) 1)
        ((char=? n #\2) 2)
        ((char=? n #\3) 3)
        ((char=? n #\4) 4)
        ((char=? n #\5) 5)
        ((char=? n #\6) 6)
        ((char=? n #\7) 7)
        ((char=? n #\8) 8)
        (else 9)))

;(exercise4)

; Exercise 5
(define (question x)
  (begin 
    (display "Is the number greater than ")
    (display x)
    (display " : (answer yes/no) ")
    (cond ((symbol=? (read)'yes) true)(true false)))
  )

(define (result x)
  (string-append "It's : "  (number->string x)))

(define (exercise5)
  ; program for the guessing game
  ; no parameter -> correct guess of the program
  (display 
   (result  
    (cond ((question 8)
           (cond ((question 12)
                  (cond ((question 14)
                         (cond ((question 15) 16)(true 15)))
                        (true (cond ((question 13) 14)(true 13)))))
                 (true 
                  (cond ((question 10)
                         (cond ((question 11) 12)(true 11)))
                        (true (cond ((question 9) 10)(true 9)))))))
          (true
           (cond ((question 4)
                  (cond ((question 6)
                         (cond ((question 7) 8)(true 7)))
                        (true (cond ((question 5) 6)(true 5)))))
                 (true 
                  (cond ((question 2)
                         (cond ((question 3) 4)(true 3)))
                        (true (cond ((question 1) 2)(true 1))))))))
    )
   ))
;(exercise5)
