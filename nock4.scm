;;nock4.scm
;;20181014Z
;;jpt4
;;nock 4K in Chez Scheme

(load "match.scm")

(define (nnoun? a)
  (or (natom? a) (ncell? a)))

(define (natom? a) 
  (if (integer? a)
      (> a 0)
      #f))

(define (ncell? a)
  (match a
   [(,a ,b) 
    (and (nnoun? a) (nnoun? b))]
   [,a #f]))

(define (ras a)
  (cond
   [(natom? a) a]
   [(ncell? a) (cons (ras (car a)) 
		     (ras (cadr a)))]))

(define (nock4 a)
  (match (ras a)
    [(,a ,b . ,c) 
     (guard (not (null? c)))
     (cons a (list (cons b c)))]
    [(,op (,a ,b))
     (guard (equal? '? op) (not (equal? a b)))
     0]
    [(,op ,a)
     (guard (natom? a))
     1]
    [else a]))

