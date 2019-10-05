;nock-tests.scm
;20191005Z
;jpt4
;Chez Scheme v9.5

(test 'wut-cell (nock4-aux '(wut 0 0)) 0)
(test 'wut-cell-ras (nock4-aux '(wut (0 0))) 0)

(test 'wut-atom (nock4-aux '(wut 0)) 1)
(test 'wut-atom-ras (nock4-aux '(wut 0)) 1)
