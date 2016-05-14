(define TAB 4)
(define TAB1 (- TAB 1))

(define // quotient)
(define % remainder)
(define ** expt)

(define ARGS-CAN-BE-FUNCS #t)
(define ARGS-CANT-BE-FUNCS #f)

(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define C-LINE 0)
(define C-POSITION 1)

(define A-NAME 0)
(define A-RULES 1)

(define R-NAME 0)
(define R-TOKEN 1)
(define R-TERMS 1)

(define ERROR_NO_FUNC_BODY   "no expression in body")
(define ERROR_NO_EOF         "no end of file")
(define ERROR_EXPR_ARENS     "arens not closed")
(define ERROR_EXPR           "expr is not correct")
(define ERROR_NO_IF_CONDS    "no conditions in if")
(define ERROR_NO_IF_ACT      "no expression in if")
(define ERR_AFTER_CONTINUOUS "uncorrect after continuous")
(define ERR_NO_CONTINUOUS    "no continuous")
(define ERROR_NO_TAG_TO      "no '->' in lambda")

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-DEFS 1)
(define TYPE-EXPRS 2)
(define TYPE-ARGS-NAMES 2)

(define ERROR_NUM_OF_ARGS "the expected number of arguments does not match the given number")
(define ERROR_UNDEFINED_VARIABLE "cannot reference undefined identifier")

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

(define-syntax not-null?
  (syntax-rules ()
    ((_ x) (not (null? x)))))

(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define (print . xs)
  (or (and (not-null? xs)
           (display (car xs))
           (newline)
           (apply print (cdr xs)))
      (newline)))

(define tokens '(#(tag-sym #(1 1) "fib")
                 #(tag-num #(1 5) 0)
                 #(tag-to #(1 7) "<-")
                 #(tag-num #(1 10) 1)
                 #(tag-sym #(2 1) "fib")
                 #(tag-sym #(2 5) "n")
                 #(tag-to #(2 7) "<-")
                 #(tag-sym #(2 10) "n")
                 #(tag-mul #(2 12) "*")
                 #(tag-sym #(2 14) "fib")
                 #(tag-lprn #(2 18) #\()
                 #(tag-sym #(2 19) "n")
                 #(tag-mns #(2 21) "-")
                 #(tag-num #(2 23) 1)
                 #(tag-rprn #(2 24) #\))
                 #(tag-sym #(4 1) "fib")
                 #(tag-num #(4 5) 5)
                 #(tag-sym #(5 1) "fib")
                 #(tag-num #(5 5) 10)))

(define (get-true-expr)
  #(expr (#(tag-true #(0 0) "#t"))))

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

(define (get-token-pos token)
  (let ((coords (get-token-coords token)))
    (vector-ref coords C-POSITION)))

(define (get-rule-list ast)
  (vector-ref ast A-RULES))

(define (get-simple-rule-token rule)
  (vector-ref rule R-TOKEN))

(define (get-simple-start-pos rule)
  (let ((func-name-token (get-simple-rule-token rule)))
    (get-token-pos func-name-token)))

(define (get-expr-start-pos func-decl)
  (let* ((rule-list        (get-rule-list func-decl))
         (func-name-rule   (car rule-list)))
    (get-simple-start-pos func-name-rule)))

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
            (if ast-elem
                (helper (cons ast-elem ast-list))
                (reverse ast-list))))

        (let ((first (apply rule args)))
          (and first
               (cons first (helper '())))))

      (define (simple-rule rule-name . tags)
        (let ((t token))
          (and (apply is-type? tags)
               (next-token)
               (vector `,rule-name `,t))))

      (define (simple-func-name name word)
        (let ((t token))
          (and (is-type? 'tag-kw)
               (eqv? (get-token-value token) word)
               (next-token)
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
                             (syntax-array start-pos args-can-be-funcs?)
                             (syntax-arg-continuous start-pos args-can-be-funcs?)
                             (and args-can-be-funcs? (syntax-expr start-pos))))
                    (larg (if (list? arg) arg (list arg))))
               (and arg (vector 'argument larg)))))

      (define (syntax-arguments start-pos args-can-be-funcs?)
        (define (helper ast-list)
          (let ((arg (syntax-argument start-pos args-can-be-funcs?)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-func-declaration start-pos args-can-be-funcs?)
        (and (start-in? start-pos)
             (let ((first-rule (simple-rule 'func-name 'tag-sym)))
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
               (let* ((tag  (get-token-tag token))
                      (op   (prior token))
                      (valid-op (and (> op 0) (or (not-null? stack) (not-null? out))))
                      (proc (syntax-func-declaration start-pos ARGS-CAN-BE-FUNCS))
                      (flag #f))
                 (and (cond (valid-op             (op-to-out op)
                                                  (set! stack 
                                                        (cons token stack)))
                            (proc                 (set! flag #t)
                                                  (set! out (cons proc out)))
                            ((eqv? tag 'tag-num)  (set! out (cons token out)))
                            ((eqv? tag 'tag-lprn) (set! stack 
                                                        (cons token stack)))
                            ((eqv? tag 'tag-rprn) (op-before-laren-to-out 0))
                            (else                 (set! start-flag #f) #f))
                      (if flag
                          (try-get)
                          (and (next-token)
                               (neq? (get-token-tag token) 'tag-end)
                               (try-get)))))))

        (define (op-before-laren-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
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
        (and (or start-flag (not-null? out))
             (ops-to-out 0)
             (vector 'expr (reverse out))))

      (define (syntax-program start-pos)
        (let ((func-decl (syntax-func-declaration start-pos ARGS-CANT-BE-FUNCS)))
          (or (and func-decl
                   (let ((func-body (syntax-func-body (get-expr-start-pos 
                                                       func-decl))))
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
