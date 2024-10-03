; Exam 2022-2023
; Section 1 - Variation and percents
; 1.1
(define (variation nb pourcentage)
  ; returns the variation based on the given number and the given percent
  ; 2 reals -> real
  (let ((taux (* nb (/ pourcentage 100))))
    (+ nb taux)))
;(variation 10 5) ;returns 10.5
;(variation 12 -10) ;returns 10.8
;(variation 8.5 13) ;returns 9.605.

; 1.2
(define (pourcentage x y)
  ; returns the variation from x to y
  ; 2 reals -> 1 real
  (* 100 (- (/ y x) 1)))
;(pourcentage 10 10.5); returns 5,
;(pourcentage 12 10.8); returns -10
;(pourcentage 8.5 9.605); returns 13

; 1.3
(define (apresNVariations x n pour)
  ; returns the value of x after n variations of the pour percent
  ; 3 reals -> 1 real
  (if (= n 0)
      x
      (apresNVariations (variation x pour) (- n 1) pour)))
;(apresNVariations 1 4 100) ;renvoie 16
;(apresNVariations 10 3 -50) ;renvoie 1.25
;(apresNVariations 1 4 100) ;renvoie 16
;(apresNVariations 10 3 -50) ;renvoie 1.25

; 1.4
(define (appliqueVariations x y)
  ; returns the value of x after applying all the percents present in the list y
  ; real, list of reals -> real
  (if (null? y)
      x
      (appliqueVariations (variation x (car y)) (cdr y))))
;(appliqueVariations 10 '(10 100 -50)) ;renvoie 11.

; 1.5
(define (pourcentagesDeVariation l)
  ; returns a list of the variations between 2 successives numbers in the list l
  ; list -> list
  (pourcentagesDeVariation1 l '()))

(define (pourcentagesDeVariation1 l s)
  ; returns a list of the variations between 2 successives numbers in the list l and stores
  ; the variations in s 
  ; 2 lists -> list
  (if (null? (cdr l)) 
      s
      (pourcentagesDeVariation1 (cdr l) (append s (list (pourcentage (car l) (cadr l)))))))

(pourcentagesDeVariation '(10 11 22 11))
