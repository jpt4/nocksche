;;nock4.scm
;;20181014Z
;;jpt4
;;nock 4K in Chez Scheme

(load "match.scm")

(define (ntuple? a) (and (pair? a) (not (null? (cdr a)))))

;#->@, due to reserved characters
(define (nop? a) (member a '(tar wut lus tis fas hax)))

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
    ;wut
    [(wut (,a ,b))
     (guard (equal? a b))
     0]
    [(wut ,a)
     (guard (natom? a))
     1]
    ;lus
    [(lus (,a ,b)) `(lus (,a ,b))]
    [(lus ,a) 
     (guard (natom? a))
     (+ 1 a)]
    ;tis
    [(tis (,a ,a)) 
     (guard (natom? a)) 0]
    [(tis (,a ,b)) 
     (guard (natom? a) (natom? b)
	    (not (equal? a b)))
     1]
    [(tis ,x) a] ;err, not an atom or cell of atoms
    ;fas
    [(fas (1 ,a)) 
     (display '1-fas) (newline)
     a]
    [(fas (2 (,a ,b))) a]
    [(fas (3 (,a ,b))) b]
    [(fas (,a ,b)) 
     (guard (even? a) (>= a 4))
     (let ([res (nock4 `(fas (,(/ a 2) ,b)))])
       (nock4 `(fas (2 ,res))))
     ]
    [(fas (,a ,b)) 
     (guard (odd? a) (>= a 5))
     (let ([res (nock4 `(fas (,(/ (- a 1) 2) ,b)))])
       (nock4 `(fas (3 ,res))))
     ]
    [(fas ,a) `(fas ,a)] ;err, not a vaild tree index
    ;hax
    [(hax (1 (,a ,b))) a]
    [(hax (,a (,b ,c))) (guard (even? a))
     (let ([res (nock4 `(fas ,(+ a 1) ,c))])
       (nock4 `(hax (,(/ a 2) (,b ,res) ,c))))]
    [(hax (,a (,b ,c))) (guard (odd? a) (>= a 3))
     (let ([res (nock4 `(fas ,(- a 1) ,c))])
       (nock4 `(hax (,(/ (- a 1) 2) (,res ,b) ,c))))]
    [(hax ,a) `(hax ,a)] ;err, not a valid hax
    [,e `(* ,a)] ;err, no pattern match, loop
    ))

