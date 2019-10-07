;;nock4.scm
;;20181014Z
;;jpt4
;;nock 4K in Chez Scheme

(load "match.scm")

(define (ntuple? a) (and (pair? a) (not (null? (cdr a)))))

(define (nop? a) (member a '(tar wut lus tis fas hax)))

(define (nnoun? a)
  (or (natom? a) (ncell? a)))

(define (natom? a) 
  (if (integer? a)
      (>= a 0)
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

;Data passed to nock4 should, as per spec, be only of type noun.
;The first pass of a nock interpreter tags the input with a tar for evaluation.
;To test primitive operations, thread via the appropriate payload, or use nock4-aux
;directly.
(define (nock4 a) (nock4-aux `(tar ,a)))

(define (nock4-aux a)
  (match (ras a)
    ;wut
    [(wut (,a ,b))
     (guard (ncell? `(,a ,b)))
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
     (guard (natom? a)) 
     0]
    [(tis (,a ,b)) 
     (guard (natom? a) (natom? b)
	    (not (equal? a b)))
     1]
    [(tis ,x) a] ;err, not an atom or cell of atoms
    ;fas
    [(fas (1 ,a)) a]
    [(fas (2 (,a ,b))) a]
    [(fas (3 (,a ,b))) b]
    [(fas (,a ,b)) 
     (guard (even? a) (>= a 4))
     (let ([res (nock4-aux `(fas (,(/ a 2) ,b)))])
       (nock4-aux `(fas (2 ,res))))]
    [(fas (,a ,b)) 
     (guard (odd? a) (>= a 5))
     (let ([res (nock4-aux `(fas (,(/ (- a 1) 2) ,b)))])
       (nock4-aux `(fas (3 ,res))))
     ]
    [(fas ,a) `(fas ,a)] ;err, not a vaild tree index
    ;hax
    [(hax (1 (,a ,b))) a]
    [(hax (,a (,b ,c))) 
     (guard (even? a))
     (let ([res (nock4-aux `(fas ,(+ a 1) ,c))])
       (nock4-aux `(hax (,(/ a 2) (,b ,res) ,c))))]
    [(hax (,a (,b ,c))) 
     (guard (odd? a) (>= a 3))
     (let ([res (nock4-aux `(fas ,(- a 1) ,c))])
       (nock4-aux `(hax (,(/ (- a 1) 2) (,res ,b) ,c))))]
    [(hax ,a) `(hax ,a)] ;err, not a valid hax
    ;s combinator
    [(tar (,a ((,b ,c) ,d))) 
     (let ([resl (nock4-aux `(tar (,a ,b ,c)))]
	   [resr (nock4-aux `(tar (,a ,d)))])
       `(,resl ,resr))]
    [(tar (,a (0 ,b))) (nock4-aux `(fas (,b ,a)))]
    [(tar (,a (1 ,b))) b]
    [(tar (,a (2 (,b ,c))))
     (let ([resl (nock4-aux `(tar (,a ,b)))]
	   [resr (nock4-aux `(tar (,a ,c)))])
       (nock4-aux `(tar (,resl ,resr))))]
    [(tar (,a (3 ,b))) 
     (let ([res (nock4-aux `(tar (,a ,b)))])
       (nock4-aux `(wut ,res)))]
    [(tar (,a (4 ,b))) 
     (let ([res (nock4-aux `(tar (,a ,b)))])
       (nock4-aux `(lus ,res)))]
    [(tar (,a (5 (,b ,c))))
     (let ([resl (nock4-aux `(tar (,a ,b)))]
	   [resr (nock4-aux `(tar (,a ,c)))])
       (nock4-aux `(tis (,resl ,resr))))]
    [(tar (,a (6 (,b (,c ,d)))))
     (let* ([resl (nock4-aux `(tar ((,c ,d) (0 ,resc))))]
	    [resc (nock4-aux `(tar ((2 3) (0 ,resr))))]
	    [resr (nock4-aux `(tar (,a (4 (4 ,b)))))])
       (nock4-aux `(tar (,a ,resl))))]
    [(tar (,a (7 (,b ,c))))
     (let ([res (nock4-aux `(tar (,a ,b)))])
       (nock4-aux `(tar (,res ,c))))]
    [(tar (,a (8 (,b ,c))))
     (let ([res (nock4-aux `(tar (,a ,b)))])
       (nock4-aux `(tar ((,res ,a) ,c))))]
    [(tar (,a (9 (,b ,c))))
     (let ([res (nock4-aux `(tar (,a ,c)))])
       (nock4-aux `(tar (,res (2 ((0 1) (0 ,b)))))))]
    [(tar (,a (10 ((,b ,c) ,d))))
     (let ([resl (nock4-aux `(tar (,a ,c)))]
	   [resr (nock4-aux `(tar (,a ,d)))])
       (nock4-aux `(hax (,resl ,resr))))]
    [(tar (,a (11 ((,b ,c) ,d))))
     (let ([resl (nock4-aux `(tar (,a ,c)))]
	   [resr (nock4-aux `(tar (,a ,d)))])
       (nock4-aux `(tar ((,resl ,resr) (0 3)))))]
    [(tar (,a (11 (,b ,c))))
     (nock4-aux `(tar (,a ,c)))]
    [,e `(* ,a)] ;err, no pattern match, loop
    ))

;nock4-dir[ect]
;Alternative Nock engine. Uses direct, whole term pattern matching reductions.
(define (nock4-dir a)
  (match (ras a)
    [(wut (,a ,b)) 0]
    [(wut ,a) 1]
    [(lus (,a ,b)) `(lus (,a ,b))]
    [(lus ,a) (guard (natom? a)) (+ 1 a)]
    [(tis (,a ,a)) (guard (natom? a) (equal? a a)) 0]
    [(tis (,a ,b)) 
     (guard (natom? a) (natom? b) (not (equal? a b))) 1]
    [(fas (1 ,a)) a]
    [(fas (2 (,a ,b))) a]
    [(fas (3 (,a ,b))) b]
    [(fas (,a ,b)) (guard (>= a 4) (even? a))
    (nock4-dir `(fas 2 ,(nock4-dir `(fas ,(/ a 2) ,b))))]
    [(fas (,a ,b)) (guard (>= a 5) (odd? a))
    (nock4-dir 
     `(fas 3 ,(nock4-dir `(fas ,(/ (- a 1) 2) ,b))))]
    [(fas ,a) `(fas ,a)]
    [(hax (1 (,a ,b))) a]
    [(hax (,a (,b ,c))) (guard (even? a))
     (nock4-dir 
      `(hax ,a (,b ,(nock4-dir `(fas ,(+ a 1) ,c))) ,c))]
    [(hax (,a (,b ,c))) (guard (odd? a) (>= a 3))
     (nock4-dir 
      `(hax ,a (,(nock4-dir `(fas ,(- a 1) ,c)) ,b) ,c))]
    [(hax ,a) `(hax ,a)]
    [(tar (,a ((,b ,c) ,d)))
     (auto-cons 
      (nock4-dir `(tar ,a ,b ,c)) (nock4-dir `(,a ,d)))]
    [(tar (,a (0 ,b0)))
     (nock4-dir `(fas (,b ,a)))]
    [(tar (,a (1 ,b))) b]
    [(tar (,a (2 (,b ,c))))
     (nock4-dir `(tar ,(nock4-dir `(tar ,a ,b)) ,(nock4-dir `(tar ,a ,c))))]
    [(tar (,a (3 ,b))) (nock4-dir `(wut tar (,a ,b)))]
    [(tar (,a (4 ,b))) (nock4-dir `(lus tar (,a ,b)))]
    [(tar (,a (5 (,b ,c))))
     (nock4-dir `(tis ,(nock4-dir `(tar ,a ,b)) ,(nock4-dir `(tar ,a ,c))))]
    [(tar (,a (6 (,b (,c ,d)))))
     (nock4-dir 
      `(tar ,a 
	    ,(nock4-dir 
	      `(tar (,c ,d) 0 
		    ,(nock4-dir 
		      `(tar (2 3) 0 
			    ,(nock4-dir 
			      `(tar ,a 4 4 ,b))))))))]
    [(tar (,a (7 (,b ,c)))) 
     (nock4-dir `(tar ,(nock4-dir `(tar ,a ,b)) ,c))]
    [(tar (,a (8 (,b ,c)))) 
     (nock4-dir `(tar (,(nock4-dir `(tar ,a ,b)) ,a) ,c))]
    [(tar (,a (9 (,b ,c))))
     (nock4-dir `(tar ,(nock4-dir `(tar ,a ,c)) 2 (0 1) 0 ,b))]
    [(tar (,a (10 ((,b ,c) ,d))))
     (nock4-dir `(hax ,b ,(nock4-dir `(tar ,a ,c)) ,(nock4-dir `(tar ,a ,d))))]
    [(tar (,a (11 ((,b ,c) ,d))))
     (nock4-dir `(tar (,(nock4-dir `(tar ,a ,c)) ,(nock4-dir `(tar ,a ,d))) 0 3))]
    [(tar (,a (11 (,b ,c)))) 
     (nock4-dir `(tar ,a ,c))]
    [(tar ,a) `(tar ,a)]
    ))

;;TODO: write fs, wt, ts, hx, ls, tr functions to bring narrow gap between
;;interpreter and spec syntax. Profile all interpreter variants for time/space/
;;ergonomic performance.
