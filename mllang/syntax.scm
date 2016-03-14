(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define tokens '(#(tag-sym (2 1) "day-of-week")
                 #(tag-num (2 13) 555)
                 #(tag-lbrk (2 17) "month")
                 #(tag-rbrk (2 23) "year")
                 #(tag-end (2 27) "eof")))

(define (get-token)
  (let ((token (car tokens)))
    (set! tokens (cdr tokens))
    token))

(define syntax-program
  (let ((errors '())
        (token (get-token))
        (ast '()))
    (lambda ()
      
      (define (next-token)
        (set! token (get-token)))
      
      (define (is-type? . types)
        (and (not (null? types))
             (or (eqv? (vector-ref token T-TAG) (car types))
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
      
      (define (simple-rule rule-name . tags)
        (let ((t token))
          (and (apply is-type? tags)
               (next-token)
               `(,rule-name ,t))))
      
      (define (syntax-array-simple)
        (define ast-list '())
        
        (let ((first-rule (simple-rule 'open-braket 'tag-lbrk)))
          (and first-rule
               (set! ast-list (cons first-rule (syntax-arguments)))
               (or (let ((last-rule (simple-rule 'close-braket 'tag-rbrk)))
                     (and last-rule
                          (set! ast-list (append ast-list (list last-rule)))))
                   (add-error ERR_NO_CLOSE_BRK))
               (list 'array-simple ast-list))))
      
      (define (syntax-continious)
        (let ((first-rule (simple-rule 'colon 'tag-cln)))
          (and first-rule
               (let ((last-rule (simple-rule 'list 'tag-sym)))
                 (list 'continious
                       (list first-rule last-rule))))))
      
      (define (syntax-arg-continious)
        (let ((cont (syntax-continious)))
          (and cont
               (or (not (syntax-argument))
                   (add-error ERR_AFTER_CONTINIOUS))
               (set! ast-list (list cont)))))
      
      (define (syntax-argument)
        (let ((arg (or (simple-rule 'simple-argument 'tag-num 'tag-sym)
                       (syntax-array-simple)
                       (syntax-arg-continious))))
          (and arg (list 'argument arg))))
      
      (define (syntax-arguments)
        (define (helper ast-list)
          (let ((arg (syntax-argument)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))
      
      (define (syntax-func-declaration)
        (let ((first-rule (simple-rule 'func-name 'tag-sym)))
          (and first-rule
               (list 'func-decl
                     (cons first-rule
                           (syntax-rule? syntax-arguments))))))
      
      (define (syntax-rule1 rule)
        (or (and (symbol? rule)
                 (eqv? rule (car t))
                 s)
            (rule)))
      
      (define (syntax-def-func ast)
        (and (syntax-rule1 'tag-sym)
             (syntax-rule* arg)
             (syntax-rule1 'tag-to)
             (syntax-rule+ expr)))
      
      (define (ast-program ast tokens)
        (or (and ast (null? tokens) (list 'program (reverse ast)))
            (apply ast-program (or (syntax-def-func ast tokens)
                                   (syntax-scheme   ast tokens)
                                   (syntax-expr     ast tokens)))))
      (syntax-func-declaration))))