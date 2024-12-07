#lang racket

(provide sum-of-squares range my-set? union more-positives? add-quotes get-304-quine)

(define sum-of-squares
  (lambda (a)
    (if (null? a)
        0
        (+ (* (car a) (car a))
           (sum-of-squares (cdr a))))))

(define range
  (lambda (a b)
    (if (>= a b)
        '()
        (cons a
              (range (+ a 1) b)))))

(define my-set?
  (lambda (a)
    (if (null? a)
        #t
        (if (contains? (car a) (cdr a))
            #f
            (my-set? (cdr a))))))

(define contains?
  (lambda (ele list)
    (if (null? list)
        #f
        (if (equal? ele (car list))
            #t
            (contains? ele (cdr list))))))

(define union
  (lambda (a b)
    (if (null? b)
        a
        (if (contains? (car b) a)
            (union a (cdr b))
            (cons (car b) (union a (cdr b)))))))
    
(define more-positives?
  (lambda (lon)
    (> (count-positives lon)
       (- (length lon) (count-positives lon)))))

(define count-positives
  (lambda (list)
    (if (null? list)
        0
        (if (> (car list) 0)
            (+ 1 (count-positives (cdr list)))
            (count-positives (cdr list))))))

(define length
  (lambda (list)
    (if (null? list)
        0
        (+ 1 (length (cdr list))))))

(define add-quotes
  (lambda (val num)
    (if (<= num 0)
        val
        (cons 'quote (list (add-quotes val (- num 1)))))))
           
; Stuff for the final quine problem

(define get-304-quine
  (lambda ()
    (nyi)))

(define eval-string
  (lambda (str)
    (let ((outp (open-output-string)))
      (parameterize ([current-output-port outp])
        (printf "~s" (eval (read (open-input-string str)) (make-base-namespace))))
      (get-output-string outp))))

(define is-quine-string?
 (lambda (str)
   (let ((result (eval-string str)))
     (if (equal? result str)
         #t
         (begin
           (printf "NOT QUINE~nIn : ~s~nOut: ~s" str result)
           #f)))))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))
