(define syntax-program
  (let ((errors '()))
    (lambda (tokens)
      (define t (car tokens))
      (define s tokens)

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
      (ast-program '() tokens))))