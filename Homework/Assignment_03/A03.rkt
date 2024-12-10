#lang racket

(provide intersection subset? relation? domain reflexive? multi-set? ms-size last all-but-last)

(define intersection
  (lambda (a b)
    (if (null? a)
        '()
        (if (contains? (car a) b)
            (cons (car a)
                  (intersection (cdr a) b))
            (intersection (cdr a) b)))))

(define contains?
  (lambda (ele list)
    (if (null? list)
        #f
        (if (equal? ele (car list))
            #t
            (contains? ele (cdr list))))))

(define subset?
  (lambda (a b)
    (if (null? a)
        #t
        (if (contains? (car a) b)
            (subset? (cdr a) b)
            #f))))

(define relation?
  (lambda (a)
    (if (my-set? a)
        (if (empty? a)
            #t
            (if (and (list? (car a))
                     (= (length (car a)) 2))
                (relation? (cdr a))
                #f))
        #f)))

(define my-set?
  (lambda (a)
    (if (list? a)
        (if (null? a)
            #t
            (if (contains? (car a) (cdr a))
                #f
                (my-set? (cdr a))))
        #f)))

(define length
  (lambda (list)
    (if (or (null? list) (not (list? list)))
        0
        (+ 1 (length (cdr list))))))

(define domain
  (lambda (a)
    (if (null? a)
        '()
        (if (real? (car (car a)))
            (if (in-range? (car (car a))
                           (domain (cdr a)))
                (domain (cdr a))
                (cons (car (car a)) (domain (cdr a))))
            (if (contains? (car (car a))
                           (domain (cdr a)))
                (domain (cdr a))
                (cons (car (car a)) (domain (cdr a))))))))

(define in-range?
  (lambda (ele range)
    (if (or (null? range) (= (length range) 1))
        #f
        (and (>= ele (car range))
             (<= ele (car (cdr range)))))))

(define range
  (lambda (a)
    (if (null? a)
        '()
        (if (real? (car (cdr (car a))))
            (if (in-range? (car (cdr (car a)))
                           (range (cdr a)))
                (range (cdr a))
                (cons (car (cdr (car a))) (range (cdr a))))
            (if (contains? (car (cdr (car a)))
                           (range (cdr a)))
                (range (cdr a))
                (cons (car (cdr (car a))) (range (cdr a))))))))

(define reflexive?
  (lambda (a)
    (and (subset? (domain a) (range a))
         (subset? (range a) (domain a)))))

(define multi-set?
  (lambda (a)
    (if (list? a)
        (if (null? a)
            #t
            (if (my-set? (unwrap-keys a))
                (need-unwrap-any? a)
                #f))
        #f)))

;;Start of multi-set helper method

(define need-unwrap?
  (lambda (a)
    (and (list? a)
         (= (length a) 2)
         (integer? (car (cdr a)))
         (> (car (cdr a)) 0))))

(define need-unwrap-any?
  (lambda (a)
    (if (null? a)
        #f
        (if (and (list? (car a))
                 (need-unwrap? (car a)))
            #t
            (need-unwrap-any? (cdr a))))))

(define unwrap-keys
  (lambda (a)
    (if (null? a)
        '()
        (if (need-unwrap? (car a))
            (cons (car (car a)) (unwrap-keys (cdr a)))
            (unwrap-keys (cdr a))))))

(define unwrap-values
  (lambda (a)
    (if (null? a)
        '()
        (if (need-unwrap? (car a))
            (cons (car (cdr (car a))) (unwrap-values (cdr a)))
            (unwrap-values (cdr a))))))

;;End of multi-set? helper method

(define ms-size
  (lambda (a)
    (sum (unwrap-values a))))

(define sum
  (lambda (a)
    (if (null? a)
        0
        (+ (car a) (sum (cdr a))))))

(define last
  (lambda (a)
    (if (null? (cdr a))
        (car a)
        (last (cdr a)))))

(define all-but-last
  (lambda (a)
    (if (null? (cdr a))
        '()
        (cons (car a) (all-but-last (cdr a))))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
