; TD 5 - 22/03/2024

; Exercise 2
(define (mirrorString word)
  ; returns the mirror of the given word (so that the word will be written backwards)
  ; string -> string
  (if (= (string-length word) 0) ""
      (string-append (mirrorString (substring word 1 (string-length word)))
                     (substring word 0 1))))

;(mirrorString "Hello")

; for a list, we can't use the same solutions as for a string :
; we can scroll from left to right, but not add to the end of the list ;
; we can add to the beginning of a list, but not browse from right to left

(define (mirrorList l)
  ; returns the mirror of l (a list containing all the elements of l in reverse order)
  ; list -> list
  (mirrorL l '()))

(define (mirrorL ll rl)
  ; returns a list containing the elements of ll in reverse order,
  ; then those of rl in the same order
  ; 2 lists -> list
  (if (null? ll) rl
      (mirrorL (cdr ll) (cons (car ll) rl))))

;(mirrorList '( 1 2 3 4 5 6))

;Exercise 3 
; Solution 1 - use mirror String
(define (palindrom1? word)
  ; returns true if the word is a palindrom (readable from both sides)
  ; String -> boolean
  (string=? word (mirrorString word)))

; Solution 2 - use of recursion
(define (palindrom2? word)
  ; returns true if the word is a palindrom (readable from both sides)
  ; String -> boolean
  (let ((lg (string-length word)))
        (or (< lg 2)
            (and (char=? (string-ref word 0) (string-ref word (- lg 1)))
                 (palindrome2? (substring word 1 (- lg 1)))))))

;(palindrome1? "laval")
;(palindrome1? "naval")
;(palindrome2? "laval")
;(palindrome2? "naval")

; Solution 1 use of mirror List
(define (palindromList1? l)
  ; returns true if the list is a palindrom (readable from both sides)
  ; List -> boolean
  (equal? l (mirrorList l)))

; Solution 2 use of recursion
(define (palindromList2? l)
  ; returns true if the list is a palindrom (readable from both sides)
  ; List -> boolean
  (equalList? l (mirrorList l)))

(define (equalList? l1 l2)
  ; returns true if l1 is equal to l2
  ; 2 lists -> boolean
  (or
   (and (null? l1) (null? l2))
   (and (not (null? l1))
        (not (null? l2))
        (char=? (car l1) (car l2))
        (equalList? (cdr l1) (cdr l2)))))
 
;(palindromList1? '(#\a #\b #\c #\d))
;(palindromList1? '(#\a #\b #\b #\a))
;(palindromList2? '(#\a #\b #\c #\d))
;(palindromList2? '(#\a #\b #\b #\a))

; Exercise 4
(define (insert n l)
  ; returns an ordered list with n inside it.
  ; we suppose that l is already sorted
  ; integer, ordered list of integers -> ordered list of integers
  (cond ((null? l) (list n))
        ((<= n (car l))
             (cons n l))
         (else (cons (car l) (insert n (cdr l))))))
;(insert 5 '(2 3 4 4 5 5 9 10))

(define (sortList l)
  ; returns a list with the same elements as l, sorted in ascending order
  ; list of integers -> ordered list of integers
  (if (null? l) l
      (insert (car l) (sortList (cdr l)))))
;(sortList '(2 5 3 10 4 4 5 5 9 10))

; Exercise 5
; First solution : no support for accentuated letters
(define (sentence1 ch)
  ; returns a list of the words of ch
  ; string -> list of strings
  (searchWord1 ch))

(define (letter? l)
  ; returns true if l is a letter
  ; character -> boolean
  (and (char-ci>=? l #\a) (char-ci<=? l #\z)))

;(letter? #\d)
;(letter? #\.)

(define (searchWord1 ch)
  ; returns the list of words of ch
  ; string -> list of strings
  (let ((lg (string-length ch)))
    (cond ((= lg 0) '())
          ((letter? (string-ref ch 0)) (readWord1 (substring ch 0 1) (substring ch 1 lg)))
          (else (searchWord1 (substring ch 1 lg))))))

(define (readWord1 l ch)
  ; returns the list of words of (string-append m ch0
  ; l contains only LETTERS
  ; characters, list -> list of strings
  (let ((lg (string-length ch)))
    (cond ((= lg 0) (list l))
          ((letter? (string-ref ch 0)) (readWord1 (string-append l (substring ch 0 1))
                                                  (substring ch 1 lg)))
          (else (cons l (searchWord1 (substring ch 1 lg)))))))


; Second solution : support for accentuated letters
; We will specify the accepted accents
(define (sentence2 ch l)
  ; returns a list of the words of ch
  ; string, list of characters -> list of strings
  (searchWord2 ch l))

(define (belongs? c l)
  ; returns true if the character c is in the list l
  ; character, list of characters -> boolean
  (and (not (null? l))
       (or (char=? c (car l))
           (belongs? c (cdr l)))))

(define (searchWord2 ch l)
  ; returns a list of the words in ch considering the separators present in l
  ; string, list of characters -> list of strings
  (let ((lg (string-length ch)))
    (cond ((= lg 0) '())
          ((belongs? (string-ref ch 0) l) (searchWord2 (substring ch 1 lg) l))
          (else (readWord2 (substring ch 0 1) (substring ch 1 lg) l)))))

(define (readWord2 c ch l)
  ; returns the list of words of (string-append c ch) considering the separators present in l
  ; c contains ONLY letters
  ; non-empty string, string, list of characters -> list of strings
  (let ((lg (string-length ch)))
    (cond ((= lg 0) (list c))
          ((belongs? (string-ref ch 0) l) (cons c (searchWord2(substring ch 1 lg) l)))
          (else (readWord2 (string-append c (substring ch 0 1)) (substring ch 1 lg) l)))))

(sentence1 "Voici, par exemple, une belle phrase : des questions ?")
(sentence2 "Voici, par exemple, une belle phrase : des questions ?" '(#\space #\. #\, #\? #\! #\: #\;))
(sentence2 "Cette deuxième solution supporte aussi les signes diacritiques comme les lettres accentuées !" '(#\space #\. #\, #\? #\! #\: #\;))
(sentence1 "Ce n'est absolument pas le cas pour la première solution.")

















