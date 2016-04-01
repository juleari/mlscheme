;(import "defs.scm")
;(import "lib.scm")
;(import "lexer.scm")
;(import "syntax.scm")

(define port (open-input-file ###PATH-TO-INPUT-FILE###))
(define tokens (tokenize-file port))
(syntax)