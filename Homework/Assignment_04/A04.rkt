#lang racket

(provide pop-song? running-sum invert combine-consec)

(define pop-song?
  (lambda (lst)
    (letrec ([state-start (lambda (lst)
                            (cond
                              [(equal? (car lst) 'verse) (state-refrain (cdr lst))]
                              [else #f]))]
             [state-refrain (lambda (lst)
                              (cond
                                [(null? lst) #f]
                                [(equal? (car lst) 'refrain) (state-verse (cdr lst))]
                                [(equal? (car lst) 'guitar-solo) (state-refrain(cdr lst))]
                                [else #f]))]
             [state-verse (lambda (lst)
                            (cond
                              [(null? lst) #f]
                              [(equal? (car lst) 'verse) (state-refrain (cdr lst))]
                              [(equal? (car lst) 'refrain) (state-end (cdr lst))]
                              [(equal? (car lst) 'guitar-solo) (state-verse (cdr lst))]
                              [else #f]))]
             [state-end (lambda (lst)
                          (cond
                            [(null? lst) #t]
                            [else #f]))])
      (state-start lst))))

(define running-sum
  (lambda (lst)
    (letrec ([handle-pair (lambda (last lst)
                         (if (null? lst)
                             '()
                             (cons (+ last (car lst)) (handle-pair (+ last (car lst)) (cdr lst)))))])
      (cons (car lst) (handle-pair (car lst) (cdr lst))))))


(define invert
  (lambda (lst)
    (let ([invert-pair (lambda (pair)
                         (cons (car (cdr pair))
                               (cons (car pair) '())))])
      (if (null? lst)
          '()
          (cons (invert-pair (car lst))
                (invert (cdr lst)))))))

(define combine-consec
  (lambda (lst)
    (letrec ([continuous? (lambda (range ele)
                            (= (cadr range) (- ele 1)))]
             [state-continuous (lambda (range lst)
                                 (cond [(null? lst) (list range)]
                                       [(continuous? range (car lst))
                                        (state-continuous (list (car range) (car lst)) (cdr lst))]
                                       [else (append (list range) (state-continuous (list (car lst) (car lst)) (cdr lst)))]))])
      (if (null? lst)
          '()
          (state-continuous (list (car lst) (car lst)) (cdr lst))))))
          

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
