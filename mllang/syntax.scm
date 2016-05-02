(define syntax
  (let ((errors '())
        (ast '()))
    (lambda ()
      (define token (get-token))

      (define (add-error error-type)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- error-type)
                    errors))
        '())

      (define (add-error-rule rule)
        (set! errors
              (cons (vector 'error: 
                            (get-token-coords token)
                            '-
                            'expected
                            `,rule)
                    errors))
        '())

      (define (print-errors)
        (if (null? errors)
            (display "SYNTAX OK!")
            (begin (display "SYNTAX ERRORS:")
                   (newline)
                   (apply print (reverse errors))))
        (newline)
        (newline))

      (define (next-token)
        (set! token (get-token)))

      (define (is-type? . types)
        (apply x-in-xs? (cons (get-token-tag token) types)))

      (define (x-in-xs? x . xs)
        (and (not-null? xs)
             (or (eqv? x (car xs))
                 (apply x-in-xs? (cons x (cdr xs))))))

      (define (start-in? start-pos)
        (> (get-token-pos token) start-pos))

      (define (syntax-rule rule-expr . args)
        (eval-i rule-expr))

      (define (syntax-rule? rule . args)
        (or (apply rule args) '()))

      (define (syntax-rule* rule . args)
        (define (helper ast-list)
          (let ((ast-elem (apply rule args)))
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-rule+ rule . args)
        (define (helper ast-list)
          (let ((ast-elem (apply rule args)))
            ;(print "syntax-rule+" ast-elem)
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))

        (let ((first (apply rule args)))
          (and first
               (cons first (helper '())))))

      (define (simple-rule rule-name . tags)
        (let ((t token))
          (and (apply is-type? tags)
               ;(print "simple-rule-before-new-token" rule-name token)
               (next-token)
               (vector `,rule-name `,t))))

      (define (simple-func-name name word)
        (let ((t token))
          (and (is-type? 'tag-kw)
               (eqv? (get-token-value token) word)
               ;(print "simple-func-before-new-token" name token)
               (next-token)
               ;(print (vector `,name `,t))
               (vector `,name `,t))))

      (define (get-first-rule start-pos first-rule-expr)
        (and (start-in? start-pos)
             (eval-i first-rule-expr)))

      (define (get-program-rule start-pos)
        `(or (,syntax-rule+ ,syntax-program ,start-pos)
             (,add-error ,ERROR_NO_FUNC_BODY)))

      (define (get-lambda-rules start-pos)
        `(append (,syntax-rule* ,(simple-rule 'simple-argument 'tag-sym) 
                                ,start-pos)
                 (or (,simple-rule 'to 'tag-to)
                     (,add-error ,ERROR_NO_TAG_TO))))

      (define (syntax-partional-rule start-pos
                                     first-rule-expr
                                     next-rules)
        (let ((first-rule (get-first-rule start-pos first-rule-expr)))
          (and first-rule
               (cons first-rule
                     (eval-i next-rules)))))

      (define (syntax-whole-rule rule-name
                                 start-pos
                                 first-rule-expr
                                 syntax-rule-type
                                 second-rule-expr
                                 error-name)
        (let ((first-rule (get-first-rule start-pos first-rule-expr)))
          (and first-rule
               (vector rule-name
                       (cons first-rule
                             (or (list (syntax-rule-type second-rule-expr
                                                         (get-simple-start-pos 
                                                          first-rule)))
                                 (add-error error-name)))))))

      (define (syntax-array-simple start-pos)
        (define ast-list '())
        (if (start-in? start-pos)
            (let ((first-rule (simple-rule 'open-braket 'tag-lbrk)))
              ;(print "syntax-array-simple" token ast-list first-rule)
              (and first-rule
                   (set! ast-list (cons first-rule
                                        (syntax-arguments (get-simple-start-pos 
                                                           first-rule)
                                                          ARGS-CANT-BE-FUNCS)))
                   (or (let ((last-rule (simple-rule 'close-braket 'tag-rbrk)))
                         (and last-rule
                              (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                       (add-error ERR_NO_CLOSE_BRK))
                   (list (vector 'array-simple ast-list))))
            ast-list))

      (define (syntax-array start-pos args-can-be-funcs?)
        (define ast-list '())
        (if (start-in? start-pos)
            (let ((first-rule (simple-rule 'open-braket 'tag-lbrk)))
              ;(print "syntax-array" token ast-list first-rule)
              (and first-rule
                   (set! ast-list (cons first-rule
                                        (syntax-arguments (get-simple-start-pos 
                                                           first-rule)
                                                          args-can-be-funcs?)))
                   (or (let ((last-rule (simple-rule 'close-braket 'tag-rbrk)))
                         (and last-rule
                              (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                       (add-error ERR_NO_CLOSE_BRK))
                   (list (vector 'array-simple ast-list))))
            ast-list))

      (define (syntax-continuous-simple start-pos)
        (syntax-whole-rule 'continuous
                           start-pos
                           `(,simple-rule 'colon 'tag-cln)
                           syntax-rule
                           `(,simple-rule 'continuous-list 'tag-sym)
                           ERR_NO_CONTINUOUS))

      (define (syntax-continuous start-pos)
        (syntax-whole-rule 'continuous
                           start-pos
                           `(,simple-rule 'colon 'tag-cln)
                           syntax-rule
                           `(,syntax-program 0)
                           ERR_NO_CONTINUOUS))

      (define (syntax-arg-continuous start-pos args-can-be-funcs?)
        (and (start-in? start-pos)
             (if args-can-be-funcs?
                 (let ((cont (syntax-continuous start-pos)))
                   (and cont (list cont)))
                 (let ((cont (syntax-continuous-simple start-pos)))
                   (and cont
                        (or (not (syntax-argument start-pos args-can-be-funcs?))
                            (add-error ERR_AFTER_continuous))
                        (list cont))))))

      (define (syntax-argument start-pos args-can-be-funcs?)
        (and (start-in? start-pos)
             (let* ((arg (or (simple-rule 'simple-argument 'tag-num 'tag-sym)
                             ;(if args-can-be-funcs?
                             ;    (syntax-array start-pos)
                             ;    (syntax-array-simple start-pos))
                             (syntax-array start-pos args-can-be-funcs?)
                             (syntax-arg-continuous start-pos args-can-be-funcs?)))
                    (larg (if (list? arg) arg (list arg))))
               ;(print "syntax-argument" arg)
               (and arg (vector 'argument larg)))))

      (define (syntax-arguments start-pos args-can-be-funcs?)
        ;(print "syntax-arguments" token args-can-be-funcs?)
        (define (helper ast-list)
          (let ((arg (syntax-argument start-pos args-can-be-funcs?)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-func-declaration start-pos args-can-be-funcs?)
        ;(print "syntax-func-declaration" token args-can-be-funcs?)
        (and (start-in? start-pos)
             (let ((first-rule (simple-rule 'func-name 'tag-sym)))
               ;(print "syntax-func-declaration-first" first-rule)
               (and first-rule
                    (vector 'func-decl
                            (cons first-rule
                                  (syntax-rule? syntax-arguments
                                                (get-simple-start-pos
                                                 first-rule)
                                                args-can-be-funcs?)))))))

      (define (syntax-func-body start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule 'func-to 'tag-to)
                               (get-program-rule start-pos)))

      (define (syntax-if-actions start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule 'then 'tag-from)
                               (get-program-rule start-pos)))

      (define (syntax-if-cond start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule 'if-cond 'tag-bor)
                               `(cons (let ((expr (,syntax-rule? ,syntax-expr
                                                                 ,start-pos)))
                                        (if (and (list? expr) (null? expr))
                                            (,get-true-expr)
                                            expr))
                                      (or (,syntax-if-actions ,start-pos)
                                          (,add-error ,ERROR_NO_IF_ACT)))))

      (define (syntax-if start-pos)
        (syntax-whole-rule 'if-expression
                           start-pos
                           `(,simple-func-name 'if-word "if")
                           syntax-rule+
                           syntax-if-cond
                           ERROR_NO_IF_CONDS))

      ;; надо сделать так, чтобы в lambda-rules передавался новый start-pos
      (define (syntax-lambda start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule 'lambda-symb 'tag-lmbd)
                               (get-lambda-rules start-pos)))

      (define (syntax-expr . args)
        ;(print "expr" token args)
        (let ((start-pos (car args)))
          (if (eq? (get-token-tag token) 'tag-end)
              (and (not-null? (cdr args))
                   (vector 'expr (list (cadr args))))
              (or (let ((arr (syntax-array start-pos ARGS-CAN-BE-FUNCS)))
                    (and arr
                         (not-null? arr)
                         (vector 'expr arr)))
                  (syntax-if start-pos)
                  ;(syntax-lambda start-pos)
                  (apply shunting-yard args)))))

      (define (shunting-yard start-pos . out)
        ;(print "shunting-yard" start-pos token out)
        (define stack '())
        (define start-flag #t)

        (define (op? t)
          (x-in-xs? (get-token-tag t)
                    'tag-bor 'tag-band 'tag-xor 'tag-and 'tag-or
                    'tag-neq 'tag-hghr 'tag-lwr 'tag-heq 'tag-leq
                    'tag-pls 'tag-mns  'tag-mul 'tag-div 'tag-eq
                    'tag-mod 'tag-rem  'tag-pow 'tag-not))

        (define (is-type? . types)
          (apply x-in-xs? (cons (get-token-tag token) types)))

        (define (x-in-xs? x . xs)
          (and (not-null? xs)
               (or (eqv? x (car xs))
                   (apply x-in-xs? (cons x (cdr xs))))))

        (define (trigonometric)
          (let ((tag (get-token-tag token))
                (val (get-token-value token)))
            (and (eqv? tag 'tag-kw)
                 (x-in-xs? val "sin" "cos" "tg" "ctg"))))

        (define (prior t)
          (let ((tag (get-token-tag t)))
            (cond ((x-in-xs? tag 'tag-band)                            2)
                  ((x-in-xs? tag 'tag-or)                              3)
                  ((x-in-xs? tag 'tag-xor)                             4)
                  ((x-in-xs? tag 'tag-and)                             5)
                  ((x-in-xs? tag 'tag-neq 'tag-eq)                     6)
                  ((x-in-xs? tag 'tag-lwr 'tag-leq 'tag-hghr 'tag-heq) 7)
                  ((x-in-xs? tag 'tag-pls 'tag-mns)                    8)
                  ((x-in-xs? tag 'tag-mul 'tag-div 'tag-mod 'tag-rem)  9)
                  ((x-in-xs? tag 'tag-pow)                            10)
                  ((or (x-in-xs? tag 'tag-not) (trigonometric))       11)
                  (else                                                0))))

        (define (try-get)
          (set! start-flag (> (get-token-pos token) start-pos))
          (and start-flag
               (let ((tag  (get-token-tag token))
                     (op   (prior token))
                     (proc (syntax-func-declaration start-pos ARGS-CAN-BE-FUNCS))
                     (flag #f))
                 ;(print "try-get" tag op proc)
                 (and (cond ((> op 0)             (op-to-out op)
                                                  (set! stack 
                                                        (cons token stack)))
                            (proc                 (set! flag #t)
                                                  (set! out (cons proc out)))
                            ((eqv? tag 'tag-num)  (set! out (cons token out)))
                            ((eqv? tag 'tag-lprn) (set! stack 
                                                        (cons token stack)))
                            ((eqv? tag 'tag-rprn) (op-before-laren-to-out 0))
                            (else                 (set! start-flag #f) #f))
                      ;(print stack out "\n")
                      (if flag
                          (try-get)
                          (and ;(print "try-get-before-new-token" token)
                           (next-token)
                           (neq? (get-token-tag token) 'tag-end)
                           (try-get)))))))

        (define (op-before-laren-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                ;(print "op-before-laren-to-out" (get-token-tag cur) "\n")
                (cond ((eqv? (get-token-tag cur) 'tag-lprn)
                       (set! stack (cdr stack)))
                      ((op? cur) (from-stack-to-out cur 
                                                    op-before-laren-to-out
                                                    p))
                      (else      (add-error ERROR_EXPR_ARENS))))))

        (define (from-stack-to-out cur callback p)
          (set! out (cons cur out))
          (set! stack (cdr stack))
          (callback p))

        (define (op-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (and (neq? (get-token-tag cur) 'tag-lprn)
                         (<= (prior cur) p))
                    (from-stack-to-out cur op-to-out p)))))

        (define (ops-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (op? cur)
                    (from-stack-to-out cur ops-to-out p)
                    (add-error ERROR_EXPR)))))

        (try-get)
        ;(print start-flag stack out)
        (and (or start-flag (not-null? out))
             (ops-to-out 0)
             (vector 'expr (reverse out))))

      (define (syntax-program start-pos)
        ;(print "syntax-program" start-pos)
        (let ((func-decl (syntax-func-declaration start-pos ARGS-CANT-BE-FUNCS)))
          (or (and func-decl
                   (let ((func-body (syntax-func-body (get-expr-start-pos 
                                                       func-decl))))
                     ;(print func-decl "syntax-program-func-body" func-body)
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (syntax-expr (get-expr-start-pos func-decl)
                                      func-decl))))
              (syntax-expr start-pos))))

      (let ((ast (syntax-rule+ syntax-program 0)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NO_EOF))
        (print-errors)
        ast))))
