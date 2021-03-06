;all-tests.scm
;20191005Z
;jpt4
;Chez Scheme v9.5

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced)))))))

(printf "accesory-tests\n")
(load "accesory-tests.scm")

(printf "nock-tests\n")
(load "nock-tests.scm")

