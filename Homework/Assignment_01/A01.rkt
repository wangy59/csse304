#lang racket

(provide interval-contains? interval-intersects? interval-union make-vec-from-points dot-product vector-magnitude distance)

(define interval-contains?
  (lambda (interval number)
     (and (>= number (car interval))
          (<= number (car (cdr interval))))
  )
)

(define interval-intersects?
  (lambda (i1 i2)
    (if (< (car i1) (car i2))
        (<= (- (car i2) (car i1)) (- (car (cdr i1)) (car i1)))
        (<= (- (car i1) (car i2)) (- (car (cdr i2)) (car i2)))
    )
  )
)

(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (cons
         (cons (get-smaller (car i1) (car i2))
               (cons (get-greater (car (cdr i1)) (car (cdr i2))) '())) '())
        (cons i1 (cons i2 '()))
    )
  )
)

(define make-vec-from-points
  (lambda (v1 v2)
    (if (or (null? v1) (null? v2))
        '()
        (cons (- (car v2) (car v1))
              (make-vec-from-points (cdr v1) (cdr v2))
        )
    )
  )
)

(define dot-product
  (lambda (v1 v2)
    (if (or (null? v1) (null? v2))
        0
        (+ (* (car v1) (car v2))
           (dot-product (cdr v1) (cdr v2))
        )
    ) 
  )
)

(define vector-magnitude
  (lambda (v)
    (sqrt (+ (+ (square (car v))
                (square (car (cdr v)))
                (square (car (cdr (cdr v))))
             )
          )
    )
  )
)

(define distance
  (lambda (p1 p2)
    (vector-magnitude (make-vec-from-points p1 p2))
  )
)

(define get-greater
  (lambda (a b)
    (if (> a b)
        a
        b
    )
  )
)

(define get-smaller
  (lambda (a b)
    (if (< a b)
        a
        b
    )
  )
)

(define square
  (lambda (n)
    (* n n)
  )
)

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))