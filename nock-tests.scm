;nock-tests.scm
;20191005Z
;jpt4
;Chez Scheme v9.5

(test 'wut-cell (nock4-aux '(wut 0 0)) 0)
(test 'wut-cell-ras (nock4-aux '(wut (0 0))) 0)

(test 'wut-atom (nock4-aux '(wut 0)) 1)
(test 'wut-atom-ras (nock4-aux '(wut 0)) 1)

(test 'lus-cell (nock4-aux '(lus (0 1))) '(lus (0 1)))
(test 'lus-atom (nock4-aux '(lus 1)) 2)

(test 'tis-cell-equal (nock4-aux '(tis (1 1))) 0)
(test 'tis-cell-not-equal (nock4-aux '(tis (1 0))) 1)
(test 'tis-atom (nock4-aux '(tis 0)) '(tis 0))

(test 'fas-1 (nock4-aux '(fas (1 2))) 2)
(test 'fas-2 (nock4-aux '(fas (2 (3 4)))) 3)
(test 'fas-3 (nock4-aux '(fas (3 (3 4)))) 4)
(test 'fas-even-4 (nock4-aux '(fas (4 ((3 4) 2)))) 3)
(test 'fas-odd-5 (nock4-aux '(fas (5 ((3 4) 2)))) 4)
(test 'fas-even-6 (nock4-aux '(fas (6 ((4 5) (6 7))))) 6)
(test 'fas-odd-7 (nock4-aux '(fas (7 ((4 5) (6 7))))) 7)
(test 'fas-atom (nock4-aux '(fas 10)) '(fas 10))

(test 'hax-1 (nock4-aux '(hax (1 (2 3)))) 2)
(test 'hax-shallow-even (nock4-aux '(hax (2 (1 [0 1])))) '[1 1])
#|
#[2 1 [0 1]]
#[1 [1 /[3 [0 1]]] [0 1]]
#[1 [1 1] [0 1]]
[1 1]
|#
(test 'hax-shallow-odd (nock4-aux '(hax [3 [1 [0 1]]])) '[0 1])
#|
#[3 1 [0 1]]
#[1 [/[2 [0 1]] 1] [0 1]]
[/[2 [0 1]] 1]
[0 1]
|#
(test 'hax-deep-even (nock4-aux '(hax (4 (1 [0 1])))) '[[1 1] 1])
#|
#[4 1 [0 1]]
#[2 [1 /[5 [0 1]]] [1 0]]
#[2 [1 /[3 /[1 [0 1]]]] [1 0]]
#[2 [1 /[3 [0 1]]] [1 0]]
#[2 [1 1] [1 0]]
#[1 [[1 1] /[3 [0 1]]] [0 1]]
[[1 1] /[3 [0 1]]]
[[1 1] 1]
#|
