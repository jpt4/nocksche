;;nock4.scm
;;20181014Z
;;jpt4
;;nock 4K in Chez Scheme

(load "match.scm")

(define (ntuple? a) (and (pair? a) (not (null? (cdr a)))))

;#->@, due to reserved characters
(define (nop? a) (member a '(* ? + = / $)))

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

(define (auto-cons a b) (list a b))  

(define (ras a)
  (cond
   [(natom? a) a]
   [(ncell? a) a]
   [(nop? a) a]
   [(ntuple? a)
    (cond
     [(null? (cddr a)) (auto-cons (ras (car a)) 
				  (ras (cadr a)))]
     [(not (null? (cddr a)))
      (auto-cons (ras (car a)) (ras (cdr a)))]
     )]))  

(define (nock4 a)
  (match (ras a)
    [(? (,a ,b))
     (guard (equal? a b))
     0]
    [(? ,a)
     (guard (natom? a))
     1]
    [(+ (,a ,b)) `(+ (,a ,b))]
    [(+ ,a) 
     (guard (natom? a))
     (+ 1 a)]
    [(= (,a ,a)) 
     (guard (natom? a)) 0]
    [(= (,a ,b)) 
     (guard (natom? a) (natom? b)
	    (not (equal? a b)))
     1]
    [(= ,x) a] ;err, not an atom or cell of atoms
    [,e `(* ,a)] ;err, no pattern match, loop
    ))

