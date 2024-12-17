#lang racket

(provide vector-append-list group-by-two group-by-n bt-leaf-sum bt-inorder-list bt-max bt-max-interior slist-map slist-reverse slist-paren-count slist-depth slist-symbols-at-depth path-to make-c...r)

(define vector-append-list
  (lambda (a b)
    (nyi)))

(define group-by-two
  (lambda (a)
    (group-by-n a 2)))

(define group-by-n
  (lambda (lst num)
    (letrec ([helper-car (lambda (l n)
                           (cond [(or (= n 0) (null? l)) '()]
                                 [else (cons (car l)
                                             (helper-car (cdr l) (- n 1)))]))]
             [helper-cdr (lambda (l n)
                           (cond [(or (= n 0) (null? l)) l]
                                 [else (helper-cdr (cdr l) (- n 1))]))])
      (cond [(null? lst) '()]
            [(null? (helper-cdr lst num)) (cons lst '())]
            [else (cons (helper-car lst num)
                        (group-by-n (helper-cdr lst num) num))]))))

(define bt-leaf-sum
  (lambda (bt)
    (cond [(integer? bt) bt]
          [else (+ (bt-leaf-sum (cadr bt)) (bt-leaf-sum (caddr bt)))])))

(define bt-inorder-list
  (lambda (bt)
    (letrec ([helper (lambda (bt)
                       (cond [(integer? bt) (list bt)]
                             [else (let ([symbol (car bt)]
                                         [left-bt (cadr bt)]
                                         [right-bt (caddr bt)])
                                     (append (helper left-bt)
                                             (list symbol)
                                             (helper right-bt)))]))])
      (filter (lambda (x) (not (integer? x))) (helper bt)))))

(define bt-max
  (lambda (bt)
    (cond [(integer? bt) bt]
          [else (let ([left-max (bt-max (cadr bt))]
                      [right-max (bt-max (caddr bt))])
                  (if (> left-max right-max)
                      left-max
                      right-max))])))

(define bt-max-interior
  (lambda (a)
    (letrec ([helper (lambda (bt)
                       (let* ([symbol (car bt)]
                              [left-bt (cadr bt)]
                              [right-bt (caddr bt)]
                              [left-rst (if (real? left-bt)
                                            (list left-bt (list '() -inf.0))
                                            (helper left-bt))]
                              [right-rst (if (real? right-bt)
                                             (list right-bt (list '() -inf.0))
                                             (helper right-bt))]
                              [left-rst-sum (car left-rst)]
                              [left-rst-max (cadr left-rst)]
                              [right-rst-sum (car right-rst)]
                              [right-rst-max (cadr right-rst)]
                              [final-max (if (< (cadr left-rst-max) (cadr right-rst-max))
                                             right-rst-max
                                             left-rst-max)]
                              [cur-sum (+ left-rst-sum right-rst-sum)])
                         (if (> cur-sum (cadr final-max))
                             (list cur-sum (list symbol cur-sum))
                             (list cur-sum final-max))))])
      (caadr (helper a)))))
                         

(define slist-map
  (lambda (a b)
    (nyi)))

(define slist-reverse
  (lambda (a)
    (nyi)))

(define slist-paren-count
  (lambda (a)
    (nyi)))

(define slist-depth
  (lambda (a)
    (nyi)))

(define slist-symbols-at-depth
  (lambda (a b)
    (nyi)))

(define path-to
  (lambda (a b)
    (nyi)))

(define make-c...r
  (lambda (str)
    (nyi)))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
