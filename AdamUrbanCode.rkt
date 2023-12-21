#lang racket

; A Combinatorial Calculator

; 1. Takes a list (either of elements or of sublists containing 2 elements each)
; 2. Filters out malformed data, depending on the conditions
; 3. Outputs correct data, depending on the function used

; It is possible to change the sample data to calculate different permutations/r_permutations/r_combinations

; Video Presentation: VIMEO: https://vimeo.com/892946513 YOUTUBE: https://youtu.be/lIUqAdvrOe0?si=4eVUI1df6faZ9NHg


;------------------------- INPUT -------------------------

; Sample data (n) or (n r)

(define permutations_list '("not an integer" -1 0 1 5 10) ) ; [n!] condition: (>= n 0)
(define r_permutations_list '( (-1 1) (1 -1) (3 5) (0 0) (1 0) (5 3) (5 4) (5 5) (7 3) )) ; [n!/(n-r)!] condition: (>= n 0) && (>= n r)
(define r_combinations_list '( (-1 1) (1 -1) (3 5) (0 0) (1 0) (5 3) (5 4) (5 5) (7 3) )) ; [n!/((n-r)!*r!)] condition: (>= n 0) && (>= n r) && (>= r 0)



;------------------------- FUNCTIONS -------------------------

; Function to get #t from a non-negative number

(define non-negative?
  (lambda (n)
    (not (negative? n))))


; Function to filter out sublists, where n < 0

(define (filter-non-negative-n lst)
  (filter
   (lambda (sublist)
     (non-negative? (car sublist))) lst))

; Function to filter out sublists, where n < 0 && r < 0

(define (filter-non-negative-n-r lst)
  (filter
   (lambda (sublist)
     (andmap non-negative? sublist)) lst))

; Function to filter out sublists, where n < r

(define (filter-n-greater-or-equal-to-r lst)
  (filter
   (lambda (sublist)
     (apply >= sublist)) lst))

; Function to get the factorial of n (n!)

(define factorial (lambda (n)
                        (if (= n 0)
                            1
                            (* n (factorial (- n 1))))))

; Function for [n!/(n-r)!]

(define (calculate-r-permutations sublist)
  (/ (factorial (car sublist)) (factorial (- (car sublist) (cadr sublist)))))

; Function for [n!/((n-r)!*r!)]

(define (calculate-r-combinations sublist)
  (/ (factorial (car sublist)) (* (factorial (- (car sublist) (cadr sublist))) (factorial (cadr sublist)))))



;------------------------- OUTPUT -------------------------

; Permutations
 
(display "Original list: ")
(displayln permutations_list)

(set! permutations_list (filter integer? permutations_list)) ; Filter out non integers

(set! permutations_list (filter non-negative? permutations_list)) ; Filter out negative integers
(display "Filtered list: ")
(displayln permutations_list)

(display "Permutations list: ")
(displayln (map factorial permutations_list)) ; Getting the factorial (permutations) of the non-negative integers
(display "\n")

; r_Permutations

(display "Original list: ")
(displayln r_permutations_list)

(set! r_permutations_list (filter-non-negative-n r_permutations_list)) ; Filter out subsets with negative n

(set! r_permutations_list (filter-n-greater-or-equal-to-r r_permutations_list)) ; Filter out subsets where r > n

(display "Filtered list: ")
(displayln r_permutations_list)

(display "r_Permutations list: ")
(displayln (map calculate-r-permutations r_permutations_list)) ; Getting the r_permutations of the filtered subsets
(display "\n")

; r_Combinations

(display "Original list: ")
(displayln r_combinations_list)

(set! r_combinations_list (filter-non-negative-n-r r_combinations_list)) ; Filter out subsets with negative n or negative r

(set! r_combinations_list (filter-n-greater-or-equal-to-r r_combinations_list)) ; Filter out subsets where r > n

(display "Filtered list: ")
(displayln r_combinations_list)

(display "r_Combinations list: ")
(displayln (map calculate-r-combinations r_combinations_list)) ; Getting the r_combinations of the filtered subsets
(display "\n")