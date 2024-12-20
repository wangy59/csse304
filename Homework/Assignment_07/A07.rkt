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
                         
(define atom?
  (lambda (x)
    (or (null? x)
        (pair? x))))

(define !atom?
  (lambda (x)
    (not (atom? x))))

(define slist-map
  (lambda (proc lst)
    (cond [(null? lst) '()]
          [else (let ([cur (car lst)])
                  (cond [(atom? cur) (append (list (slist-map proc cur))
                                             (slist-map proc (cdr lst)))]
                        [else (append (list (proc cur))
                                      (slist-map proc (cdr lst)))]))])))

(define slist-reverse
  (lambda (lst)
    (cond [(or (null? lst) (null? (cdr lst))) lst]
          [else (let ([cur (car lst)])
                  (cond [(atom? cur) (append (slist-reverse (cdr lst))
                                             (list (slist-reverse cur)))]
                        [else (append (slist-reverse (cdr lst))
                                      (list cur))]))])))

(define slist-paren-count
  (lambda (lst)
    (cond [(null? lst) 2]
          [(atom? (car lst)) (+ (slist-paren-count (car lst))
                                (slist-paren-count (cdr lst)))]
          [else (slist-paren-count (cdr lst))])))

(define slist-depth
  (lambda (lst)
    (letrec ([clean-lst (filter atom? lst)]
             [greatest (lambda (lst)
                         (cond [(null? lst) '()]
                               [(null? (cdr lst)) (car lst)]
                               [(> (car lst) (cadr lst)) (greatest (cons (car lst) (cddr lst)))]
                               [else (greatest (cdr lst))]))])
      (cond [(null? clean-lst) 1]
            [else (+ 1 (greatest (map slist-depth clean-lst)))]))))

(define slist-symbols-at-depth
  (lambda (lst dep)
    (let ([clean-lst (filter atom? lst)]
          [clean-sym (filter !atom? lst)])
      (cond [(= dep 1) clean-sym]
            [(null? clean-lst) '()]
            [else (apply append (map (lambda (lst) (slist-symbols-at-depth lst (sub1 dep))) clean-lst))]))))

(define path-to
  (lambda (lst sym)
    (letrec ([last (lambda (lst)
                     (if (null? (cdr lst))
                         (car lst)
                         (last (cdr lst))))]
             [helper (lambda (lst)
                       (cond [(null? lst) (list #f)]
                             [(atom? (car lst)) (if (car (helper (car lst)))
                                                    (cons 'car (helper (car lst)))
                                                    (cons 'cdr (helper (cdr lst))))]
                             [else (if (equal? (car lst) sym)
                                       (list 'car)
                                       (cons 'cdr (helper (cdr lst))))]))])
      (if (equal? (last (helper lst)) #f)
          #f
          (helper lst)))))
    
 
(define make-c...r
  (lambda (str)
    (letrec ([str-lst (string->list str)]
             [helper (lambda (l)
                       (cond [(null? l) '()]
                             [(equal? (car l) #\a) (cons car (helper (cdr l)))]
                             [(equal? (car l) #\d) (cons cdr (helper (cdr l)))]
                             [else 'error]))])
      (apply compose1 (helper str-lst)))))
      

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
