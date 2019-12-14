#lang cKanren

(require
  cKanren/attributes
  cKanren/miniKanren)

(define (type A [depth 0])
  (conde
    ((== A 'Set))
    ((term A depth))
    ((fresh (B C)
       (type B depth)
       (type C (+ 1 depth))
       (== A `(Pi ,B ,C))))))

(define (rangeo x m)
  (if (zero? m)
      fail
      (conde
        ((== x (- m 1)))
        ((rangeo x (- m 1))))))

(define (term s [depth 0])
  (conde
    ((rangeo s depth))
    ((symbol s))
    ((fresh (t u)
       (term t depth)
       (term u depth)
       (== s `(app ,t ,u))))
    ((fresh (t)
       (term t (+ 1 depth))
       (== s `(lam ,t))))))

(define (listof goal l)
  (conde
    ((== l '()))
    ((fresh (hd tl)
       (goal hd)
       (listof goal tl)
       (conso hd tl l)))))

(define (print-each l)
  (if (null? l)
      '()
      (begin
        (displayln (car l))
        (print-each (cdr l)))))

(print-each (run 10 (A) (term A)))

(print-each (run 3 (l) (listof type l)))
