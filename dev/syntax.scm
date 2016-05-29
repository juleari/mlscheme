(define TAB 4)
(define TAB1 (- TAB 1))

(define // quotient)
(define % remainder)
(define ** expt)

(define ARGS-CAN-BE-EXPRS #t)
(define ARGS-CANT-BE-EXPRS #f)
(define ARGS-CAN-BE-FUNCS #t)
(define ARGS-CANT-BE-FUNCS #f)
(define ARGS-CAN-BE-CONT #t)
(define ARGS-CANT-BE-CONT #f)

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
(define ERR_NO_CLOSE_BRK     "no close braket")
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


(define tokens '(#(tag-sym #(1 1) "bisection")
                 #(tag-sym #(1 11) "f")
                 #(tag-sym #(1 13) "a")
                 #(tag-sym #(1 15) "b")
                 #(tag-sym #(1 17) "e")
                 #(tag-to #(1 19) "<-")
                 #(tag-sym #(2 5) "sign")
                 #(tag-num #(2 10) 0)
                 #(tag-to #(2 12) "<-")
                 #(tag-num #(2 15) 0)
                 #(tag-sym #(3 5) "sign")
                 #(tag-sym #(3 10) "x")
                 #(tag-to #(3 12) "<-")
                 #(tag-kw #(3 15) "if")
                 #(tag-bor #(3 18) "|")
                 #(tag-sym #(3 20) "x")
                 #(tag-hghr #(3 22) ">")
                 #(tag-num #(3 24) 0)
                 #(tag-from #(3 26) "->")
                 #(tag-num #(3 29) 1)
                 #(tag-bor #(4 18) "|")
                 #(tag-from #(4 26) "->")
                 #(tag-num #(4 29) -1)
                 #(tag-sym #(6 5) "mid")
                 #(tag-sym #(6 9) "a")
                 #(tag-sym #(6 11) "b")
                 #(tag-to #(6 13) "<-")
                 #(tag-sym #(7 9) "x")
                 #(tag-to #(7 11) "<-")
                 #(tag-sym #(7 14) "a")
                 #(tag-pls #(7 16) "+")
                 #(tag-lprn #(7 18) #\()
                 #(tag-sym #(7 19) "b")
                 #(tag-mns #(7 21) "-")
                 #(tag-sym #(7 23) "a")
                 #(tag-rprn #(7 24) #\))
                 #(tag-rem #(7 26) "/")
                 #(tag-num #(7 28) 2)
                 #(tag-kw #(9 9) "if")
                 #(tag-bor #(9 13) "|")
                 #(tag-lprn #(9 15) #\()
                 #(tag-kw #(9 16) "abs")
                 #(tag-sym #(9 17) "f")
                 #(tag-sym #(9 19) "x")
                 #(tag-rprn #(9 20) #\))
                 #(tag-leq #(9 22) "<=")
                 #(tag-sym #(9 25) "e")
                 #(tag-from #(9 36) "->")
                 #(tag-sym #(9 39) "x")
                 #(tag-bor #(10 13) "|")
                 #(tag-lprn #(10 15) #\()
                 #(tag-sym #(10 16) "sign")
                 #(tag-sym #(10 21) "f")
                 #(tag-dot #(10 22) #\.)
                 #(tag-lbrk #(10 23) #\[)
                 #(tag-sym #(10 24) "b")
                 #(tag-rbrk #(10 25) #\])
                 #(tag-rprn #(10 26) #\))
                 #(tag-eq #(10 28) "=")
                 #(tag-lprn #(10 30) #\()
                 #(tag-sym #(10 31) "sign")
                 #(tag-sym #(10 36) "f")
                 #(tag-dot #(10 37) #\.)
                 #(tag-lbrk #(10 38) #\[)
                 #(tag-sym #(10 39) "x")
                 #(tag-rbrk #(10 40) #\])
                 #(tag-rprn #(10 41) #\))
                 #(tag-from #(10 43) "->")
                 #(tag-sym #(10 46) "mid")
                 #(tag-sym #(10 50) "a")
                 #(tag-sym #(10 52) "x")
                 #(tag-bor #(11 13) "|")
                 #(tag-from #(11 39) "->")
                 #(tag-sym #(11 42) "mid")
                 #(tag-sym #(11 46) "x")
                 #(tag-sym #(11 48) "b")
                 #(tag-kw #(13 5) "if")
                 #(tag-bor #(13 8) "|")
                 #(tag-sym #(13 10) "f")
                 #(tag-dot #(13 11) #\.)
                 #(tag-lbrk #(13 12) #\[)
                 #(tag-sym #(13 13) "a")
                 #(tag-rbrk #(13 14) #\])
                 #(tag-eq #(13 16) "=")
                 #(tag-num #(13 18) 0)
                 #(tag-from #(13 20) "->")
                 #(tag-sym #(13 23) "a")
                 #(tag-bor #(14 8) "|")
                 #(tag-sym #(14 10) "f")
                 #(tag-dot #(14 11) #\.)
                 #(tag-lbrk #(14 12) #\[)
                 #(tag-sym #(14 13) "b")
                 #(tag-rbrk #(14 14) #\])
                 #(tag-eq #(14 16) "=")
                 #(tag-num #(14 18) 0)
                 #(tag-from #(14 20) "->")
                 #(tag-sym #(14 23) "b")
                 #(tag-bor #(15 8) "|")
                 #(tag-from #(15 18) "->")
                 #(tag-sym #(15 21) "mid")
                 #(tag-sym #(15 25) "a")
                 #(tag-sym #(15 27) "b")
                 #(tag-sym #(17 1) "bisection")
                 #(tag-kw #(17 11) "cos")
                 #(tag-num #(17 15) -3.0)
                 #(tag-num #(17 20) 0.0)
                 #(tag-num #(17 24) 0.001)))

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

(define (get-true-expr)
  (vector 'expr (list #(tag-true #(0 0) #t))))

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
       (or (equal? x (car xs))
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
                             (or (list
                                  (apply syntax-rule-type
                                         (append
                                          (list second-rule-expr
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
                                                          ARGS-CANT-BE-EXPRS
                                                          ARGS-CANT-BE-FUNCS
                                                          ARGS-CAN-BE-CONT)))
                   (or (let ((last-rule (simple-rule start-pos
                                                     'close-braket
                                                     'tag-rbrk)))
                         (and last-rule
                              (set! ast-list (append ast-list 
                                                     (list last-rule)))))
                       (add-error ERR_NO_CLOSE_BRK))
                   (list (vector 'array-simple ast-list))))
            ast-list))

      (define (syntax-array start-pos args-can-be-exprs?)
        (define ast-list '())
        (and (start-in? start-pos)
             (let ((first-rule (simple-rule start-pos 'open-braket 'tag-lbrk)))
               (and first-rule
                    ;(print 'syntax-array ARGS-CAN-BE-CONT)
                    (set! ast-list (cons first-rule
                                         (syntax-arguments (get-simple-start-pos 
                                                            first-rule)
                                                           args-can-be-exprs?
                                                           args-can-be-exprs?
                                                           ARGS-CAN-BE-CONT)))
                    (or (let ((last-rule (simple-rule start-pos
                                                      'close-braket
                                                      'tag-rbrk)))
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

      (define (syntax-arg-continuous start-pos args-can-be-exprs?)
        (and (start-in? start-pos)
             (if args-can-be-exprs?
                 (let ((cont (syntax-continuous start-pos)))
                   (and cont (list cont)))
                 (let ((cont (syntax-continuous-simple start-pos)))
                   (and cont
                        (or (not (syntax-argument start-pos
                                                  args-can-be-exprs?
                                                  args-can-be-exprs?
                                                  ARGS-CANT-BE-CONT))
                            (add-error ERR_AFTER_continuous))
                        (list cont))))))

      (define (syntax-apply start-pos)
        (syntax-whole-rule 'apply
                           start-pos
                           `(,simple-rule ,start-pos 'apply-dot 'tag-dot)
                           syntax-rule-
                           syntax-argument
                           '(ARGS-CAN-BE-EXPRS ARGS-CAN-BE-FUNCS ARGS-CAN-BE-CONT)
                           ERR_INCORRECT_APPLY))

      (define (syntax-simple-or-apply start-pos)
        (and (start-in? start-pos)
             (let ((name (simple-rule start-pos
                                      'simple-argument
                                      'tag-sym
                                      'tag-kw
                                      'tag-str
                                      'tag-num)))
               (and name
                    (let* ((name-token (get-simple-rule-token name))
                           (:apply (syntax-apply (get-token-pos name-token))))
                      (if :apply
                          (make-syntax-expr (vector 'func-decl
                                                    (list (vector 'func-name
                                                                  name-token)
                                                          (vector 'argument
                                                                  (list :apply)))))
                          name))))))

      (define (syntax-argument start-pos
                               args-can-be-exprs?
                               args-can-be-funcs?
                               args-can-be-cont?)
        (and (start-in? start-pos)
             (let* ((arg (or (and (not args-can-be-funcs?)
                                  (syntax-simple-or-apply start-pos))
                             (syntax-array start-pos args-can-be-exprs?)
                             (and args-can-be-cont?
                                  (syntax-arg-continuous start-pos
                                                         args-can-be-exprs?))
                             (and args-can-be-exprs? (syntax-apply start-pos))
                             (and args-can-be-exprs?
                                  (syntax-expr start-pos
                                               (not args-can-be-cont?)))))
                    (larg (if (list? arg) arg (list arg))))
               ;(print 'syntax-argument token (not args-can-be-funcs?) arg)
               (and arg (vector 'argument larg)))))

      (define (syntax-arguments start-pos
                                args-can-be-exprs?
                                args-can-be-funcs?
                                args-can-be-cont?)
        (define (helper ast-list)
          (let ((arg (syntax-argument start-pos
                                      args-can-be-exprs?
                                      args-can-be-funcs?
                                      args-can-be-cont?)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))

      (define (syntax-func-lambda-decl start-pos
                                       name-rule-args
                                       rule-name
                                       args-can-be-exprs?
                                       args-can-be-cont?)
        (and (start-in? start-pos)
             (let ((first-rule (apply simple-rule
                                      (cons start-pos name-rule-args))))
               (and first-rule
                    (vector rule-name
                            (cons first-rule
                                  (syntax-rule? syntax-arguments
                                                (get-simple-start-pos first-rule)
                                                args-can-be-exprs?
                                                ARGS-CANT-BE-FUNCS
                                                args-can-be-cont?)))))))

      (define (syntax-func-declaration start-pos
                                       args-can-be-exprs?
                                       args-can-be-cont?)
        (syntax-func-lambda-decl start-pos
                                 '(func-name tag-sym)
                                 'func-decl
                                 args-can-be-exprs?
                                 args-can-be-cont?))

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
                                                                 ,start-pos
                                                                 ,ARGS-CAN-BE-CONT)))
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
        (cons (syntax-arguments start-pos
                                ARGS-CANT-BE-EXPRS
                                ARGS-CANT-BE-FUNCS
                                ARGS-CAN-BE-CONT)
              (or (syntax-lambda-body start-pos)
                  (add-error ERROR_NO_TAG_TO))))

      (define (syntax-lambda start-pos)
        (let ((func-decl (syntax-func-lambda-decl start-pos
                                                  '(func-name tag-lmbd)
                                                  'lambda-func-decl
                                                  ARGS-CANT-BE-EXPRS
                                                  ARGS-CAN-BE-CONT)))
          (and func-decl
               (let* ((new-start-pos (get-expr-start-pos func-decl))
                      (func-body (syntax-lambda-body new-start-pos)))
                 (or (and func-body
                          (vector 'lambda-func
                                  (cons func-decl func-body))))))))

      (define (syntax-expr start-pos args-can-be-cont? . func-decl?)
        ;(print 'syntax-expr token args)
        (if (eq? (get-token-tag token) 'tag-end)
            (and (not-null? func-decl?)
                 (make-syntax-expr (car func-decl?)))
            (make-syntax-expr (or (syntax-array start-pos ARGS-CAN-BE-FUNCS)
                                  (syntax-if start-pos)
                                  (syntax-lambda start-pos)
                                  (apply shunting-yard
                                         (append (list start-pos
                                                       args-can-be-cont?)
                                               func-decl?))))))
    
      (define (shunting-yard start-pos args-can-be-cont? . out)
        ;(print 'shunting-yard token start-pos out)
        (define stack '())
        (define start-flag #t)

        (define (op? t)
          (or (x-in-xs? (get-token-tag t)
                        'tag-bor 'tag-band 'tag-xor 'tag-and 'tag-or
                        'tag-neq 'tag-hghr 'tag-lwr 'tag-heq 'tag-leq
                        'tag-pls 'tag-mns  'tag-mul 'tag-div 'tag-eq
                        'tag-mod 'tag-rem  'tag-pow 'tag-not 'tag-conc)
              (is-func-call? t)))

        (define (is-type? . types)
          (apply x-in-xs? (cons (get-token-tag token) types)))

        (define (trigonometric)
          (let ((tag (get-token-tag token))
                (val (get-token-value token)))
            (and (eqv? tag 'tag-kw)
                 (x-in-xs? val "sin" "cos" "tg" "ctg"))))

        (define (is-unar-func? val)
          (or (x-in-xs? val "zero?" "odd?" "even?" "null?" "not" "abs" "!")
              (trigonometric)))

        (define (is-binar-func? val)
          (x-in-xs? val "eq?" "eqv?" "equal?" "gcd" "lcm"))

        (define (is-nar-func? val)
          (x-in-xs? val "and" "or"))

        (define (is-func-call? t)
          (let ((tag (get-token-tag t))
                (val (get-token-value t)))
            (and (eqv? tag 'tag-kw)
                 (or (is-unar-func? val)
                     (is-binar-func? val)
                     (is-nar-func? val)))))

        (define (prior t)
          (let ((tag (get-token-tag t)))
            (cond ((x-in-xs? tag 'tag-band)                            2)
                  ((x-in-xs? tag 'tag-or)                              3)
                  ((x-in-xs? tag 'tag-xor)                             4)
                  ((x-in-xs? tag 'tag-and)                             5)
                  ((x-in-xs? tag 'tag-neq 'tag-eq)                     6)
                  ((x-in-xs? tag 'tag-lwr 'tag-leq 'tag-hghr 'tag-heq) 7)
                  ((x-in-xs? tag 'tag-pls 'tag-mns 'tag-conc)          8)
                  ((x-in-xs? tag 'tag-mul 'tag-div 'tag-mod 'tag-rem)  9)
                  ((x-in-xs? tag 'tag-pow)                            10)
                  ((x-in-xs? tag 'tag-not)                            11)
                  ((is-func-call? token)                              12)
                  (else                                                0))))

        (define (unar? t)
          (x-in-xs? t 'tag-not))

        (define (try-get)
          (set! start-flag (> (get-token-pos token) start-pos))
          (and start-flag
               (let* ((tag  (get-token-tag token))
                      (val  (get-token-value token))
                      (op   (prior token))
                      (valid-op (and (> op 0)
                                     (or (not-null? stack)
                                         (not-null? out)
                                         (is-unar-func? val))))
                      (to-out? (and (x-in-xs? tag
                                              'tag-num
                                              'tag-true
                                              'tag-fls
                                              'tag-str)
                                    (or (null? out)
                                        (> (prior (car (reverse out))) 0)
                                        (not-null? stack))))
                      (proc (syntax-func-declaration start-pos
                                                     ARGS-CAN-BE-FUNCS
                                                     args-can-be-cont?))
                      (flag #f)
                      (end-flag #f))
                 (and (cond (valid-op             (op-to-out op)
                                                  (set! stack 
                                                        (cons token stack)))
                            (proc                 (set! flag #t)
                                                  (set! out (cons proc out)))
                            (to-out?              (set! out (cons token out)))
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
                                             (cdns (vector 'argument
                                                           (list res-out))
                                                   (get-rule-list def)))
                                def)
                         def)))
            (shunting-alg)))

      (define (syntax-scheme start-pos)
        (syntax-whole-rule 'scheme
                           start-pos
                           `(,simple-func-name 'scheme-word "scheme")
                           syntax-rule-
                           simple-rule
                           '(scheme-expr tag-schm)
                           ERROR_NO_IF_CONDS))

      (define (syntax-program start-pos)
        (let ((func-decl (syntax-func-declaration start-pos
                                                  ARGS-CANT-BE-FUNCS
                                                  ARGS-CAN-BE-CONT)))
          (or (and func-decl
                   (let* ((new-start-pos (get-expr-start-pos func-decl))
                          (func-body (syntax-func-body new-start-pos)))
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (let ((func-args (syntax-arguments new-start-pos
                                                            ARGS-CAN-BE-EXPRS
                                                            ARGS-CANT-BE-FUNCS
                                                            ARGS-CAN-BE-CONT)))
                           (syntax-expr new-start-pos
                                        ARGS-CAN-BE-CONT
                                        (append-to-rule-list func-decl
                                                             func-args))))))
              (syntax-scheme start-pos)
              (syntax-expr start-pos ARGS-CAN-BE-CONT))))

      (let ((ast (syntax-rule+ syntax-program 0)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NO_EOF))
        (print-errors)
        ast))))
