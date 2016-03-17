(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define ERROR_NO_FUNC_BODY "there is no body")
(define ERROR_NO_EOF       "no end of file")

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

(define tokens '(#(tag-sym #(2 1) "day-of-week")
                 #(tag-sym #(2 13) "day")
                 #(tag-sym #(2 17) "month")
                 #(tag-sym #(2 23) "year")
                 #(tag-to #(2 28) "<-")
                 #(tag-sym #(3 5) "a")
                 #(tag-to #(3 7) "<-")
                 #(tag-sym #(4 5) "e")
                 #(tag-end #(2 27) "eof")))

(define (get-token)
  (if (null? tokens)
      #(tag-end #(2 27) "eof")
      (let ((token (car tokens)))
        (set! tokens (cdr tokens))
        token)))

(define (get-token-tag token)
  (vector-ref token T-TAG))

(define (get-token-coords token)
  (vector-ref token T-COORDS))

(define (get-token-value token)
  (vector-ref token T-VALUE))

(define syntax
  (let ((errors '())
        (token (get-token))
        (ast '()))
    (lambda ()
      
      (define (add-error error-type)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- error-type)
                    errors))
        '())
      
      (define (add-error-rule rule)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- 'expected `,rule)
                    errors))
        '())
      
      (define (print-errors)
        (if (null? errors)
            (display "SYNTAX OK!")
            (begin (display "SYNTAX ERRORS:")
                   (newline)
                   (display errors)))
        (newline)
        (newline))
      
      (define (next-token)
        (set! token (get-token)))
      
      (define (is-type? . types)
        (and (not (null? types))
             (or (eqv? (get-token-tag token) (car types))
                 (apply is-type? (cdr types)))))
      
      (define (syntax-rule? rule)
        (or (rule) '()))
      
      (define (syntax-rule* rule)
        (define (helper ast-list)
          (let ((ast-elem (rule)))
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))
        (helper '()))
      
      (define (syntax-rule+ rule)
        (define (helper ast-list)
          (let ((ast-elem (rule)))
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))
        
        (let ((first (rule)))
          (and first
               (cons first (helper '())))))
      
      (define (simple-rule rule-name . tags)
        (let ((t token))
          (and (apply is-type? tags)
               (next-token)
               (vector `,rule-name `,t))))
      
      (define (syntax-array-simple)
        (define ast-list '())
        
        (let ((first-rule (simple-rule 'open-braket 'tag-lbrk)))
          (and first-rule
               (set! ast-list (cons first-rule (syntax-arguments)))
               (or (let ((last-rule (simple-rule 'close-braket 'tag-rbrk)))
                     (and last-rule
                          (set! ast-list (append ast-list (list last-rule)))))
                   (add-error ERR_NO_CLOSE_BRK))
               (vector 'array-simple ast-list))))
      
      (define (syntax-continious)
        (let ((first-rule (simple-rule 'colon 'tag-cln)))
          (and first-rule
               (let ((last-rule (simple-rule 'list 'tag-sym)))
                 (vector 'continious
                         (list first-rule last-rule))))))
      
      (define (syntax-arg-continious)
        (let ((cont (syntax-continious)))
          (and cont
               (or (not (syntax-argument))
                   (add-error ERR_AFTER_CONTINIOUS))
               (set! ast-list (list cont)))))
      
      (define (syntax-argument)
        (let* ((arg (or (simple-rule 'simple-argument 'tag-num 'tag-sym)
                        (syntax-array-simple)
                        (syntax-arg-continious)))
               (larg (if (list? arg) arg (list arg))))
          (and arg (vector 'argument larg))))
      
      (define (syntax-arguments)
        (define (helper ast-list)
          (let ((arg (syntax-argument)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))
      
      (define (syntax-func-declaration)
        (let ((first-rule (simple-rule 'func-name 'tag-sym)))
          (and first-rule
               (vector 'func-decl
                       (cons first-rule
                             (syntax-rule? syntax-arguments))))))
      
      (define (syntax-func-body)
        (let ((first-rule (simple-rule 'func-to 'tag-to)))
          (and first-rule
               (cons first-rule
                     (or (syntax-rule+ syntax-program)
                         (add-error ERROR_NO_FUNC_BODY))))))
      
      (define (syntax-expr)
        (and (neq? (get-token-tag token) 'tag-end)
             (get-token))
        #(expr))
      
      (define (syntax-program)
        (let ((func-decl (syntax-func-declaration)))
          (or (and func-decl
                   (let ((func-body (syntax-func-body)))
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (syntax-expr))))
              (syntax-expr))))
      
      ; (syntax-rule+ syntax-program)
      ; (syntax-func-declaration)
      (let ((ast (syntax-program)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NOT_EOF))
        (print-errors)
        ast))))