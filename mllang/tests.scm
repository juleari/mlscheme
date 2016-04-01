(define (run-test func args correct-ans)
  (equal? (apply func args)
          correct-ans))

(define input-ports-list  (map input-dir  (list ###TESTS-INPUT###)))
(define output-ports-list (map output-dir (list ###TESTS-OUTPUT###)))

(define tokens (list))

(map (lambda (input-port output-port)
       (set! tokens (tokenize-file input-port))
       (display (toString (syntax)) output-port)
       (close-output-port output-port))
     input-ports-list
     output-ports-list)