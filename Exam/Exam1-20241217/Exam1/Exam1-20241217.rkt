#lang racket

(provide remove-duplicates
         subsequences
         complete
         matrix-vector-mul)

;---------------------|
;                     |
; Recurse 1   25 pts. |
;                     |
;---------------------|

;; Write a function that removes all duplicate elements from a
;; list, keeping only the FIRST occurrence. Here are some examples:

;; (remove-duplicates '(1 2 3 2 1 4)) ; => '(1 2 3 4)
;; (remove-duplicates '(2 2 2 2))     ; => '(2)
;; (remove-duplicates '(7 8 9 6 3 5)) ; => '(7 8 9 6 3 5)

;; NOTE: You are only allowed to use simple procedures such as member, filter, reverse
;; to solve this problem. 
;; You may not use built-in racket methods such as list->set or Racket's remove-duplicates.
;; Their use WILL RESULT in 0 points for this problem.
;; If you want to use another built-in racket method ask me first!

(define remove-duplicates
  (lambda (lst)
    (letrec ([helper (lambda (x lst)
                       (filter (lambda (a) (not (equal? a x))) lst))])
      (cond [(null? lst) '()]
            [else (cons (car lst) (remove-duplicates (helper (car lst) (cdr lst))))]))))

;---------------------|
;                     |
; Recurse 2   25 pts. |
;                     |
;---------------------|

;; Write a function subsequences that returns a list of all subsequences of a given list.
;; NOTE: If you have taken MA276, this is identical to a powerset of a given list.
;; There is a certain pattern that you need to recognize. It may be helpful to sketch out
;; a few scenarios on paper.

;; NOTE 2: You are NOT ALLOWED to use the built in racket method combinations.
;; NOTE 3: Order does not matter.
;; Note 4: Therefore you CANNOT have both (2 1) and (1 2) in the output of (subsequences '(1 2))

;; Here are some examples:
;; (subsequences '(1 2)) ; => '(() (1) (2) (1 2))
;; (subsequences '(1 2 3)) ; => '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))
;; (subsequences '(a b c d)) ; => '(() (a) (b) (c) (d) (a b) (a c) (a d) (b c) (b d) (c d)
;;                                  (a b c) (a b d) (b c d) (a b c d))

(define subsequences
  (lambda (a)
    (letrec ([add-head (lambda (a l) (map (lambda (n) (cons a n)) l))]
             [divide (lambda (l) (map list l))]
             [find-all (lambda (n l)
                         (cond [(= n 0) (list '())]
                               [(= n 1) (divide l)]
                               [(= n (length l)) (list l)]
                               [else (append (add-head (car l) (find-all (sub1 n) (cdr l))) (find-all n (cdr l)))]))]
             [helper (lambda (n l)
                       (cond [(= n (length l)) (list l)]
                             [else (append (find-all n l) (helper (add1 n) l))]))])
      (helper 0 a))))

;---------------------|
;                     |
; Recurse 3   25 pts. |
;                     |
;---------------------|

;; Write a function complete, which returns the depth, i.e. the path length from the top-level list
;; to any empty list, if the list passed to it is such that each sublist
;; has two lists and each empty list has the same distance (or depth) from the top-level list. It
;; returns #f otherwise. You may assume you are not passed an empty list.

;; Here are some examples:
;; (complete '())                   ; => You will not recieve an empty list.
;; (complete '(()()))               ; => 1
;; (complete '((()())(()())))       ; => 2
;; (complete '((()())()))           ; => #f
;; (complete '((()())(()())(()()))) ; => #f

(define complete
  (lambda (ls)
    (letrec ([helper (lambda (ls)
                       (cond [(null? ls) (list 0 #t)]
                             [(not (= (length ls) 2)) (list 0 #f)]
                             [else (let* ([left (car ls)]
                                          [right (cadr ls)]
                                          [left-rst (helper left)]
                                          [right-rst (helper right)]
                                          [left-rst-h (car left-rst)]
                                          [right-rst-h (car right-rst)]
                                          [left-rst-f (cadr left-rst)]
                                          [right-rst-f (cadr right-rst)])
                                     (cond [(and left-rst-f right-rst-f)
                                            (list (add1 left-rst-h) (= left-rst-h right-rst-h))]
                                           [else (list 0 #f)]))]))])
      (if (cadr (helper ls))
          (car (helper ls))
          (cadr (helper ls))))))

;---------------------|
;                     |
; Map, Apply  25 pts. |
;                     |
;---------------------|

;; Write a function that takes a matrix in the form of
;; ((row) (row) ... (row)) and multiplies it by a vector.

;; For 100% credit all of the recursion must be accomplished by the use of map and
;; apply in your code.

;; In other words, this function MAY NOT be explicitly recursive,
;; mutually recursive or call helper functions written by you that are recursive.

;; Here is an example of matrix-vector multiplication:
;; [1 2 3]   [11]   [(1*11) + (2*12) + (3*13)]   [74]
;; [4 5 6] * [12] = [(4*11) + (5*12) + (6*13)] = [182]
;; [7 8 9]   [13]   [(7*11) + (8*12) + (9*13)]   [290]

;; HINT: Remember a map can take in two lists like so (map operation lst1 lst2)

(define matrix-vector-mul
  (lambda (matrix vector)
    (let ([helper (lambda (l) (map (lambda (x y) (* x y)) l vector))])
      (map (lambda (l) (apply + l)) (map helper matrix)))))


;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))