#lang racket

(provide minimize-interval-list exists? product best remove-last)

; this first one is probably the hardest in the set
; so if you get stuck I'd try the later ones
(define minimize-interval-list
  (lambda (a)
    (letrec ([sorted-a (sort a #:key car <)]
             [merge (lambda (lst)
                      (cond
                        [(null? (cdr lst)) lst]
                        [(<= (caadr lst) (cadar lst))
                         (merge (cons (merge-range (car lst) (cadr lst))
                                      (cddr lst)))]
                        [else (cons (car lst) (merge (cdr lst)))]))]
             [merge-range (lambda (r1 r2)
                            (list (true-returner < (car r1) (car r2))
                                  (true-returner > (cadr r1) (cadr r2))))]
             [true-returner (lambda (proc x y)
                              (if (proc x y)
                                  x
                                  y))])
      (merge sorted-a))))

(define exists?
  (lambda (proc lst)
    (ormap proc lst)))

(define best
  (lambda (proc lst)
    (letrec ([highest (lambda (proc ele lst)
                        (cond
                          [(null? lst) ele]
                          [(not ele) (highest proc (car lst) (cdr lst))]
                          [(> (proc (car lst)) (proc ele)) (highest proc (car lst) (cdr lst))]
                          [else (highest proc ele (cdr lst))]))])
      (highest proc #f lst))))

(define product
  (lambda (a b)
    (letrec ([product-xy1 (lambda (lst ele)
                            (if (null? lst)
                                '()
                                (cons (list (car lst) ele) (product-xy1 (cdr lst) ele))))])
      (if (null? b)
          '()
          (append (product-xy1 a (car b)) (product a (cdr b)))))))

(define remove-last
  (lambda (ele lst)
    (letrec ([lcar (lambda (lst)
                     (car (list-tail lst (- (length lst) 1))))]
             [lcdr (lambda (lst)
                     (if (null? (cdr lst))
                         '()
                         (cons (car lst) (lcdr (cdr lst)))))]
             [helper (lambda (lst)
                       (cond
                         [(null? lst) '()]
                         [(equal? (lcar lst) ele) (lcdr lst)]
                         [else (append (helper (lcdr lst)) (list (lcar lst)))]))])
      (helper lst))))
      

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
