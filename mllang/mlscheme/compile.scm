;(import "defs.scm")
;(import "lib.scm")
;(import "lexer.scm")
;(import "syntax.scm")
;(import "semantic.scm")
;(import "generate.scm")

(define (prepare-gen-file)
  (let ((lib-funcs-file (open-input-file ###PATH-TO-GENBASE-FILE###)))
    (while (not (eof-object? (peek-char lib-funcs-file)))
           (display (read-char lib-funcs-file) gen-file))))

(define gen-file (open-output-file ###PATH-TO-GEN-FILE###))
(prepare-gen-file)
(define port (open-input-file ###PATH-TO-INPUT-FILE###))
(define tokens (tokenize-file port))
(define ast (syntax))
(define model (semantic ast))
(generate model)
