#lang racket

; To use these tests:
; Click "Run" in the upper right
; (r)

; If you find errors in your code, fix them, save your file, click the "Run" button again, and type (r)
; You can run a specific group of tests using (run-tests group-name)

(require "../testcode-base.rkt")
(require "Exam1-20241217.rkt")
(provide get-weights get-names individual-test test)

;; Sort each sublist internally. Empty subsets remain empty after sorting.
(define canonicalize-subsets
  (lambda (subsets)
    (if (or (null? subsets) (equal? subsets 'nyi))
        '()
        (map (lambda (sub) (sort sub <)) subsets))))

;; Checks if every element of s1 appears in s2
;; Works even if some subsets (elements) are empty.
(define subset?
  (lambda (s1 s2)
  (cond
    [(null? s1) #t]
    [(member (car s1) s2) (subset? (cdr s1) s2)]
    [else #f])))

;; Checks if two sets of subsequences are the same as sets, ignoring order.
;; Empty subsets, if present, will be included and compared accordingly.
(define equal-sublists?
  (lambda (s1 s2)
  (let* ([c1 (canonicalize-subsets s1)]
         [c2 (canonicalize-subsets s2)])
    (and (= (length c1) (length c2))
         (subset? c1 c2)
         (subset? c2 c1)))))

(define test (make-test ; (r)

  (remove-duplicates equal?
         [(remove-duplicates '()) '() 1]
         [(remove-duplicates '(1)) '(1) 1]
         [(remove-duplicates '(1 2 3 4 5)) '(1 2 3 4 5) 1]
         [(remove-duplicates '(1 1 1 1 1)) '(1) 1]
         [(remove-duplicates '(1 2 3 2 1 4 5 3 4)) '(1 2 3 4 5) 1]
         [(remove-duplicates '(1 (2 3) 1 (2 3) 4)) '(1 (2 3) 4) 1]
         [(remove-duplicates '((1 2) (1 2) 3 4 3)) '((1 2) 3 4) 1]
         [(remove-duplicates '(1 2 3 4 5 1 2)) '(1 2 3 4 5) 1]
         [(remove-duplicates '(a b c b a d)) '(a b c d) 1]
         [(remove-duplicates '(1 2 3 2 1 4)) '(1 2 3 4) 1])

  (subsequences equal-sublists?
      [(subsequences '()) '(()) 1]
      [(subsequences '(1)) '(() (1)) 1]
      [(subsequences '(1 2)) '(() (1) (2) (1 2)) 1]
      [(subsequences '(1 2 3)) '(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)) 1]
      [(subsequences '(4)) '(() (4)) 1]
      [(subsequences '(0 1)) '(() (0) (1) (0 1)) 1]
      [(subsequences '(1 2 3 4)) '(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3) (4) (1 4) (2 4) (1 2 4) (3 4) (1 3 4) (2 3 4) (1 2 3 4)) 1]
      [(subsequences '(2 4 6)) '(() (2) (4) (2 4) (6) (2 6) (4 6) (2 4 6)) 1]
      [(subsequences '(0 -1 2))'(() (0) (-1) (0 -1) (2) (0 2) (-1 2) (0 -1 2)) 1]
      [(subsequences '(1 2 3 4 5))
       '(() 
         (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3)
         (4) (1 4) (2 4) (1 2 4) (3 4) (1 3 4) (2 3 4) (1 2 3 4)
         (5) (1 5) (2 5) (1 2 5) (3 5) (1 3 5) (2 3 5) (1 2 3 5)
         (4 5) (1 4 5) (2 4 5) (1 2 4 5) (3 4 5) (1 3 4 5) (2 3 4 5) (1 2 3 4 5)) 1])

  (matrix-vector-mul equal?
    [(matrix-vector-mul '((1 2 3) (4 5 6) (7 8 9)) '(1 0 -1)) '(-2 -2 -2) 1]
    [(matrix-vector-mul '((2 0) (0 2)) '(5 5)) '(10 10) 1]
    [(matrix-vector-mul '((3)) '(10)) '(30) 1]
    [(matrix-vector-mul '((1 2 3) (4 5 6) (7 8 9)) '(11 12 13)) '(74 182 290) 1]
    [(matrix-vector-mul '((10 10 10 10 10)) '(1 1 1 1 1)) '(50) 1]
    [(matrix-vector-mul '((0 0) (0 0)) '(1 2)) '(0 0) 1]
    [(matrix-vector-mul '((1) (2) (3)) '(10)) '(10 20 30) 1]
    [(matrix-vector-mul '((5)) '(2)) '(10) 1])
  
  (complete equal?
    [(complete '(()())) 1 1]            
    [(complete '((()())(()()))) 2 1]   
    [(complete '((()())())) #f 1]      
    [(complete '((()())(()())(()()))) #f 1]
    [(complete '(((()())(()()))((()())(()())))) 3 1]
    [(complete '(((()())(()()))((()())(())))) #f 1])
    
))

(implicit-run test) ; run tests as soon as this file is loaded
