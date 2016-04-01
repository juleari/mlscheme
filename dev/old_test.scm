(define (append-base-dir . path)
  (apply string-append (cons ###BASE-DIR### path)))

(define (input-dir file-name)
  (open-input-file (append-base-dir ###DIR-IN### file-name)))

(define (output-dir file-name)
  (open-output-file (append-base-dir ###DIR-OUT### file-name)))

(define (run-test func args correct-ans)
  (equal? (apply func args)
          correct-ans))

(define input-ports-list  (map input-dir  (list ###TESTS###)))
(define output-ports-list (map output-dir (list ###TESTS###)))

(define tokens (list))

(map (lambda (input-port output-port)
       (set! tokens (tokenize-file input-port))
       (display (toString (syntax)) output-port)
       (close-output-port output-port))
     input-ports-list
     output-ports-list)