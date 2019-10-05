;;non-nock test suite
;;20191005Z
;;jpt4
;;Chez Scheme v9.5
;;Test everything outside of (nock4) and (nock4-aux)

(test 'ntuple?-2-tuple (ntuple? '(1 2)) #t)
(test 'ntuple?-3-tuple (ntuple? '(1 2 3)) #t)
(test 'ntuple?-not-pair (ntuple? '1) #f)
(test 'ntuple?-null-cdr (ntuple? '(1)) #f)
(test 'ntuple?-empty-list (ntuple?' ()) #f)



