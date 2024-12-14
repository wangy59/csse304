#lang racket

(provide curry2 curried-compose compose make-list-c reverse-it map-by-position empty-BST empty-BST? BST-insert BST-inorder BST? BST-element BST-left BST-right BST-insert-nodes BST-contains? BST-height let->application let*->let qsort sort-list-of-symbols)

(define curry2
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a b c)))))

(define curried-compose
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a (b c))))))

(define compose
  (lambda insts
    (lambda (tar)
      (foldr (lambda (inst rst)
               (inst rst))
             tar
             insts))))

(define make-list-c
  (lambda (n)
    (lambda (ele)
      (letrec ([helper (lambda (x)
                         (if (<= x 0)
                             '()
                             (cons ele (helper (- x 1)))))])
        (helper n)))))

(define reverse-it
  (lambda (lst)
    (foldl (lambda (ele rst)
             (cons ele rst))
           '()
           lst)))

(define map-by-position
  (lambda (fn-lst arg-lst)
    (if (null? fn-lst)
        '()
        (cons ((car fn-lst) (car arg-lst))
              (map-by-position (cdr fn-lst) (cdr arg-lst))))))

(define empty-BST
  (lambda ()
    '()))

(define empty-BST?
  (lambda (a)
    (and (list? a) (null? a))))

(define BST-insert
  (lambda (num bst)
    (cond
      [(empty-BST? bst) (list num '() '())]
      [(= (BST-element bst) num) bst]
      [(< (BST-element bst) num) (list (BST-element bst)
                                       (BST-left bst)
                                       (BST-insert num (BST-right bst)))]
      [(> (BST-element bst) num) (list (BST-element bst)
                                       (BST-insert num (BST-left bst))
                                       (BST-right bst))])))

(define BST-inorder
  (lambda (bst)
    (cond
      [(empty-BST? bst) '()]
      [else (append (BST-inorder (BST-left bst))
                    (list (BST-element bst))
                    (BST-inorder (BST-right bst)))])))

(define BST?
  (lambda (a)
    (cond
      [(null? a) #t]
      [(or (not (list? a))
           (not (= (length a) 3))
           (not (real? (car a)))
           (not (list? (cadr a)))
           (not (list? (caddr a)))) #f]
      [else     
        (letrec ([cur (BST-element a)]
                 [cur-left (BST-left a)]
                 [cur-right (BST-right a)]
                 [extreme-value (lambda (proc bst)
                                  (if (null? (proc bst))
                                      (BST-element bst)
                                      (extreme-value proc (proc bst))))])
          (and (or (null? cur-left)
                   (and (< (BST-element cur-left) cur)
                        (< (extreme-value BST-right cur-left) cur)))
               (or (null? cur-right)
                   (and (> (BST-element cur-right) cur)
                        (> (extreme-value BST-left cur-right) cur)))
               (BST? cur-left)
               (BST? cur-right)))])))

(define BST-element
  (lambda (a)
    (car a)))

(define BST-left
  (lambda (a)
    (cadr a)))

(define BST-right
  (lambda (a)
    (caddr a)))

(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums)
        bst
        (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?
  (lambda (bst num)
    (cond
      [(or (not (BST? bst)) (empty-BST? bst)) #f]
      [(= (BST-element bst) num) #t]
      [(< (BST-element bst) num) (BST-contains? (BST-right bst) num)]
      [(> (BST-element bst) num) (BST-contains? (BST-left bst) num)])))

(define BST-height
  (lambda (bst)
    (if (null? bst)
        -1
        (letrec ([left-height (BST-height (BST-left bst))]
                 [right-height (BST-height (BST-right bst))])
          (if (> left-height right-height)
              (+ 1 left-height)
              (+ 1 right-height))))))

(define let->application
  (lambda (a)
    (let ([ids (map car (cadr a))]
          [values (map cadr (cadr a))]
          [let-body (caddr a)])
      (append (list (list 'lambda ids let-body)) values))))

(define let*->let
  (lambda (a)
    (letrec ([body1 (cadr a)]
             [body2 (caddr a)]
             [helper (lambda (b)
                       (if (null? (cdr b))
                           (append (list 'let (list (car b)))
                                   (list body2))
                           (append (list 'let (list (car b)))
                                   (list (helper (cdr b))))))])
      (append (helper body1)))))

(define qsort
  (lambda (proc lst)
    (cond [(< (length lst) 2) lst]
          [else (let* ([pivot (car lst)]
                       [rest (cdr lst)]
                       [filter-proc (lambda (x) (proc pivot x))]
                       [nfilter-proc (lambda (x) (not (filter-proc x)))]
                       [ts (filter filter-proc rest)]
                       [fs (filter nfilter-proc rest)])
                  (append (qsort proc fs) (list pivot) (qsort proc ts)))])))
                  

(define sort-list-of-symbols
  (lambda (lst)
    (let ([str-lst (map symbol->string lst)])
      (map string->symbol (sort str-lst string<?)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
