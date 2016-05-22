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
(define ERR_INCORRECT_APPLY  "apply has no arguments")

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

(define-syntax cdns
  (syntax-rules ()
    ((_ x xs) (append xs (list x)))))

(define (print . xs)
  (or (and (not-null? xs)
           (display (car xs))
           (newline)
           (apply print (cdr xs)))
      (newline)))

(define tokens '(#(tag-sym #(1 1) "replace")
                 #(tag-sym #(1 9) "pred?")
                 #(tag-sym #(1 15) "proc")
                 #(tag-lbrk #(1 20) #\[)
                 #(tag-rbrk #(1 21) #\])
                 #(tag-to #(1 31) "<-")
                 #(tag-lbrk #(1 34) #\[)
                 #(tag-rbrk #(1 35) #\])
                 #(tag-sym #(2 1) "replace")
                 #(tag-sym #(2 9) "pred?")
                 #(tag-sym #(2 15) "proc")
                 #(tag-lbrk #(2 20) #\[)
                 #(tag-sym #(2 22) "x")
                 #(tag-cln #(2 24) #\:)
                 #(tag-sym #(2 26) "xs")
                 #(tag-rbrk #(2 29) #\])
                 #(tag-to #(2 31) "<-")
                 #(tag-kw #(3 5) "if")
                 #(tag-bor #(3 8) "|")
                 #(tag-sym #(3 10) "pred?")
                 #(tag-sym #(3 16) "x")
                 #(tag-from #(3 18) "->")
                 #(tag-lbrk #(3 21) #\[)
                 #(tag-sym #(3 23) "proc")
                 #(tag-dot #(3 27) #\.)
                 #(tag-lbrk #(3 28) #\[)
                 #(tag-sym #(3 29) "x")
                 #(tag-rbrk #(3 30) #\])
                 #(tag-cln #(3 32) #\:)
                 #(tag-sym #(3 34) "replace")
                 #(tag-sym #(3 42) "pred?")
                 #(tag-sym #(3 48) "proc")
                 #(tag-sym #(3 53) "xs")
                 #(tag-rbrk #(3 56) #\])
                 #(tag-bor #(4 8) "|")
                 #(tag-from #(4 18) "->")
                 #(tag-lbrk #(4 21) #\[)
                 #(tag-sym #(4 23) "x")
                 #(tag-cln #(4 25) #\:)
                 #(tag-sym #(4 27) "replace")
                 #(tag-sym #(4 35) "pred?")
                 #(tag-sym #(4 41) "proc")
                 #(tag-sym #(4 46) "xs")
                 #(tag-rbrk #(4 49) #\])
                 #(tag-sym #(6 1) "replace")
                 #(tag-kw #(6 9) "zero?")
                 #(tag-lmbd #(7 9) "\\")
                 #(tag-sym #(7 11) "x")
                 #(tag-from #(7 13) "->")
                 #(tag-sym #(7 16) "x")
                 #(tag-pls #(7 18) "+")
                 #(tag-num #(7 20) 1)
                 #(tag-lbrk #(8 9) #\[)
                 #(tag-num #(8 11) 0)
                 #(tag-num #(8 13) 1)
                 #(tag-num #(8 15) 2)
                 #(tag-num #(8 17) 3)
                 #(tag-num #(8 19) 0)
                 #(tag-rbrk #(8 21) #\])
                 #(tag-sym #(10 1) "replace")
                 #(tag-sym #(10 9) "odd?")
                 #(tag-lmbd #(11 9) "\\")
                 #(tag-sym #(11 11) "x")
                 #(tag-from #(11 13) "->")
                 #(tag-sym #(11 16) "x")
                 #(tag-mul #(11 18) "*")
                 #(tag-num #(11 20) 2)
                 #(tag-lbrk #(12 9) #\[)
                 #(tag-num #(12 11) 1)
                 #(tag-num #(12 13) 2)
                 #(tag-num #(12 15) 3)
                 #(tag-num #(12 17) 4)
                 #(tag-num #(12 19) 5)
                 #(tag-num #(12 21) 6)
                 #(tag-rbrk #(12 23) #\])
                 #(tag-sym #(14 1) "replace")
                 #(tag-lmbd #(14 9) "\\")
                 #(tag-sym #(14 11) "x")
                 #(tag-from #(14 13) "->")
                 #(tag-num #(14 16) 0)
                 #(tag-hghr #(14 18) ">")
                 #(tag-sym #(14 20) "x")
                 #(tag-sym #(15 9) "exp")
                 #(tag-lbrk #(16 9) #\[)
                 #(tag-num #(16 11) 0)
                 #(tag-num #(16 13) 1)
                 #(tag-num #(16 15) -1)
                 #(tag-num #(16 18) 2)
                 #(tag-num #(16 20) -2)
                 #(tag-num #(16 23) 3)
                 #(tag-num #(16 25) -3)
                 #(tag-rbrk #(16 27) #\])))

(define (get-true-expr)
  (vector 'expr (list #(tag-true #(0 0) "#t"))))

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

(define (get-rule-name rule)
  (vector-ref rule R-NAME))

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

(define (x-in-xs? x . xs)
  (and (not-null? xs)
       (or (eqv? x (car xs))
           (apply x-in-xs? (cons x (cdr xs))))))

(define (append-to-rule-list rule xs)
  (vector (get-rule-name rule)
          (append (get-rule-list rule) xs)))

(define (make-syntax-expr terms)
  (and terms
       (vector 'expr (if (list? terms)
                    terms
                    (list terms)))))

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

      (define (start-in? start-pos)
        (> (get-token-pos token) start-pos))

      (define (syntax-rule rule-expr . args)
        ;(print 'syntax-rule rule-expr)
        (eval-i rule-expr))

      (define (syntax-rule? rule . args)
        (or (apply rule args) '()))

      (define (syntax-rule* rule . args)
        (define (helper ast-list)
          (let ((ast-elem (apply rule args)))
            ;(print 'syntax-rule* ast-elem)
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

      (define (simple-rule start-pos rule-name . tags)
        (and (start-in? start-pos)
             (let ((t token))
               (and (apply is-type? tags)
                    (next-token)
                    (vector `,rule-name `,t)))))

      (define (simple-func-name name word)
        (let ((t token))
          (and (is-type? 'tag-kw)
               (equal? (get-token-value token) word)
               (next-token)
               (vector `,name `,t))))

      (define (get-first-rule start-pos first-rule-expr)
        (and (start-in? start-pos)
             (eval-i first-rule-expr)))

      (define (get-program-rule start-pos)
        `(or (,syntax-rule+ ,syntax-program ,start-pos)
             (,add-error ,ERROR_NO_FUNC_BODY)))

      (define (syntax-lambda-body start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'func-from 'tag-from)
                               (get-program-rule start-pos)))

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
                                 second-rule-args
                                 error-name)
        (let ((first-rule (get-first-rule start-pos first-rule-expr)))
          ;(print 'syntax-whole-rule first-rule)
          (and first-rule
               (vector rule-name
                       (cons first-rule
                             (or (list (apply syntax-rule-type
                                              (append (list second-rule-expr
                                                            (get-simple-start-pos first-rule))
                                                      second-rule-args)))
                                 (and error-name (add-error error-name))))))))

      (define (syntax-array-simple start-pos)
        (define ast-list '())
        (if (start-in? start-pos)
            (let ((first-rule (simple-rule start-pos 'open-braket 'tag-lbrk)))
              (and first-rule
                   (set! ast-list (cons first-rule
                                        (syntax-arguments (get-simple-start-pos 
                                                           first-rule)
                                                          ARGS-CANT-BE-FUNCS)))
                   (or (let ((last-rule (simple-rule start-pos 'close-braket 'tag-rbrk)))
                         (and last-rule
                              (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                       (add-error ERR_NO_CLOSE_BRK))
                   (list (vector 'array-simple ast-list))))
            ast-list))

      (define (syntax-array start-pos args-can-be-funcs?)
        (define ast-list '())
        (and (start-in? start-pos)
             (let ((first-rule (simple-rule start-pos 'open-braket 'tag-lbrk)))
               (and first-rule
                    (set! ast-list (cons first-rule
                                         (syntax-arguments (get-simple-start-pos 
                                                            first-rule)
                                                           args-can-be-funcs?)))
                    (or (let ((last-rule (simple-rule start-pos 'close-braket 'tag-rbrk)))
                          (and last-rule
                               (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                        (add-error ERR_NO_CLOSE_BRK))
                    (list (vector 'array-simple ast-list))))))

      (define (syntax-continuous-simple start-pos)
        ;(print 'syntax-continuous-simple start-pos)
        (syntax-whole-rule 'continuous
                           start-pos
                           `(,simple-rule ,start-pos 'colon 'tag-cln)
                           syntax-rule-
                           simple-rule
                           '(continuous-list tag-sym)
                           ERR_NO_CONTINUOUS))

      (define (syntax-continuous start-pos)
        ;(print 'syntax-continuous start-pos)
        (syntax-whole-rule 'continuous
                           start-pos
                           `(,simple-rule ,start-pos 'colon 'tag-cln)
                           syntax-rule-
                           syntax-program
                           '()
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

      (define (syntax-apply start-pos)
        (syntax-whole-rule 'apply
                           start-pos
                           `(,simple-rule ,start-pos 'apply-dot 'tag-dot)
                           syntax-rule-
                           syntax-argument
                           '(#t)
                           ERR_INCORRECT_APPLY))

      (define (syntax-simple-or-apply start-pos)
        (and (start-in? start-pos)
             (let ((name (simple-rule start-pos 'simple-argument 'tag-sym 'tag-kw)))
               (and name
                    (let* ((name-token (get-simple-rule-token name))
                           (:apply (syntax-apply (+ (get-token-pos name-token) 1))))
                      (if :apply
                          (vector 'func-decl
                                  (list (vector 'func-name name-token)
                                        (vector 'argument (list :apply))))
                          name))))))

      (define (syntax-argument start-pos args-can-be-funcs?)
        (and (start-in? start-pos)
             (let* ((arg (or (and args-can-be-funcs? (syntax-simple-or-apply start-pos))
                             (and (not args-can-be-funcs?)
                                  (simple-rule start-pos 'simple-argument 'tag-sym 'tag-kw))
                             (simple-rule start-pos 'simple-argument 'tag-num)
                             (syntax-array start-pos args-can-be-funcs?)
                             (syntax-arg-continuous start-pos args-can-be-funcs?)
                             (and args-can-be-funcs? (syntax-apply start-pos))
                             (and args-can-be-funcs? (syntax-expr start-pos))))
                    (larg (if (list? arg) arg (list arg))))
               (and arg (vector 'argument larg)))))

      (define (syntax-arguments start-pos args-can-be-funcs?)
        (define (helper ast-list)
          (let ((arg (syntax-argument start-pos args-can-be-funcs?)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-func-lambda-decl start-pos name-rule-args rule-name args-can-be-funcs?)
        (and (start-in? start-pos)
             (let ((first-rule (apply simple-rule (cons start-pos name-rule-args))))
               (and first-rule
                    (vector rule-name
                            (cons first-rule
                                  (syntax-rule? syntax-arguments
                                                (get-simple-start-pos first-rule)
                                                args-can-be-funcs?)))))))

      (define (syntax-func-declaration start-pos args-can-be-funcs?)
        (syntax-func-lambda-decl start-pos '(func-name tag-sym) 'func-decl args-can-be-funcs?))

      (define (syntax-func-body start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'func-to 'tag-to)
                               (get-program-rule start-pos)))

      (define (syntax-if-actions start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'then 'tag-from)
                               (get-program-rule start-pos)))

      (define (syntax-if-cond start-pos)
        (syntax-partional-rule start-pos
                               `(,simple-rule ,start-pos 'if-cond 'tag-bor)
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
                           '()
                           ERROR_NO_IF_CONDS))

      (define (syntax-rule- rule . args)
        (apply rule args))

      (define (get-lambda-rules start-pos)
        ;(print 'get-lambda-rules start-pos)
        (cons (syntax-arguments start-pos ARGS-CANT-BE-FUNCS)
              (or (syntax-lambda-body start-pos)
                  (add-error ERROR_NO_TAG_TO))))

      (define (syntax-lambda start-pos)
        (let ((func-decl (syntax-func-lambda-decl start-pos
                                                  '(func-name tag-lmbd)
                                                  'lambda-func-decl
                                                  ARGS-CANT-BE-FUNCS)))
          (and func-decl
               (let* ((new-start-pos (get-expr-start-pos func-decl))
                      (func-body (syntax-lambda-body new-start-pos)))
                 (or (and func-body
                          (vector 'lambda-func
                                  (cons func-decl func-body))))))))

      (define (syntax-expr start-pos . func-decl?)
        ;(print 'syntax-expr token args)
        (if (eq? (get-token-tag token) 'tag-end)
            (and (not-null? func-decl?)
                 (make-syntax-expr (car func-decl?)))
            (make-syntax-expr (or (syntax-array start-pos ARGS-CAN-BE-FUNCS)
                                  (syntax-if start-pos)
                                  (syntax-lambda start-pos)
                                  (apply shunting-yard (cons start-pos func-decl?))))))
    
      (define (shunting-yard start-pos . out)
        ;(print 'shunting-yard token start-pos out)
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
                  ((x-in-xs? tag 'tag-not)                            11)
                  ((trigonometric)                                    12)
                  (else                                                0))))

        (define (try-get)
          (set! start-flag (> (get-token-pos token) start-pos))
          (and start-flag
               (let* ((tag  (get-token-tag token))
                      (op   (prior token))
                      (valid-op (and (> op 0) (or (not-null? stack) (not-null? out))))
                      (proc (syntax-func-declaration start-pos ARGS-CAN-BE-FUNCS))
                      (flag #f)
                      (end-flag #f))
                 (and (cond (valid-op             (op-to-out op)
                                                  (set! stack 
                                                        (cons token stack)))
                            (proc                 (set! flag #t)
                                                  (set! out (cons proc out)))
                            ((eqv? tag 'tag-num)  (set! out (cons token out)))
                            ((eqv? tag 'tag-lprn) (set! stack 
                                                        (cons token stack)))
                            ((eqv? tag 'tag-rprn) (or (and (not-null? stack)
                                                           (op-before-laren-to-out 0))
                                                      (and (set! end-flag #t)
                                                           (set! start-flag #f))))
                            (else                 (set! start-flag #f)
                                                  #f))
                      (cond (end-flag #f)
                            (flag     (try-get))
                            (else     (and (next-token)
                                            (neq? (get-token-tag token) 'tag-end)
                                            (try-get))))))))

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
                         (>= (prior cur) p))
                    (from-stack-to-out cur op-to-out p)))))

        (define (ops-to-out p)
          (if (not-null? stack)
              (let ((cur (car stack)))
                (if (op? cur)
                    (from-stack-to-out cur ops-to-out p)
                    (add-error ERROR_EXPR)))))

        (define (shunting-alg)
          (try-get)
          (and (or start-flag (not-null? out))
               (ops-to-out 0)
               (reverse out)))

        (define def #f)
        (if (and (not-null? out)
                 (> (get-token-pos token) start-pos)
                 (not (> (prior token) 0)))
            (begin (set! def (car out))
                   (set! out '())
                   (let ((res-out (shunting-alg)))
                     (if res-out
                         (begin (vector-set! def
                                             R-TERMS
                                             (cdns (vector 'argument (list res-out))
                                                   (get-rule-list def)))
                                def)
                         def)))
            (shunting-alg)))

      (define (syntax-program start-pos)
        (let ((func-decl (syntax-func-declaration start-pos ARGS-CANT-BE-FUNCS)))
          (or (and func-decl
                   (let* ((new-start-pos (get-expr-start-pos func-decl))
                          (func-body (syntax-func-body new-start-pos)))
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (let ((func-args (syntax-arguments new-start-pos ARGS-CAN-BE-FUNCS)))
                           (syntax-expr new-start-pos
                                        (append-to-rule-list func-decl
                                                             func-args))))))
              (syntax-expr start-pos))))

      (let ((ast (syntax-rule+ syntax-program 0)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NO_EOF))
        (print-errors)
        ast))))
