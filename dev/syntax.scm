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


(define tokens '(#(tag-sym #(44 1) "replace")
                 #(tag-sym #(44 9) "pred?")
                 #(tag-sym #(44 15) "proc")
                 #(tag-lbrk #(44 20) #\[)
                 #(tag-rbrk #(44 21) #\])
                 #(tag-to #(44 31) "<-")
                 #(tag-lbrk #(44 34) #\[)
                 #(tag-rbrk #(44 35) #\])
                 #(tag-sym #(45 1) "replace")
                 #(tag-sym #(45 9) "pred?")
                 #(tag-sym #(45 15) "proc")
                 #(tag-lbrk #(45 20) #\[)
                 #(tag-sym #(45 22) "x")
                 #(tag-cln #(45 24) #\:)
                 #(tag-sym #(45 26) "xs")
                 #(tag-rbrk #(45 29) #\])
                 #(tag-to #(45 31) "<-")
                 #(tag-kw #(46 5) "if")
                 #(tag-bor #(46 8) "|")
                 #(tag-sym #(46 10) "pred?")
                 #(tag-sym #(46 16) "x")
                 #(tag-from #(46 18) "->")
                 #(tag-lbrk #(46 21) #\[)
                 #(tag-sym #(46 23) "proc")
                 #(tag-dot #(46 27) #\.)
                 #(tag-lbrk #(46 28) #\[)
                 #(tag-sym #(46 29) "x")
                 #(tag-rbrk #(46 30) #\])
                 #(tag-cln #(46 32) #\:)
                 #(tag-sym #(46 34) "replace")
                 #(tag-sym #(46 42) "pred?")
                 #(tag-sym #(46 48) "proc")
                 #(tag-sym #(46 53) "xs")
                 #(tag-rbrk #(46 56) #\])
                 #(tag-bor #(47 8) "|")
                 #(tag-from #(47 18) "->")
                 #(tag-lbrk #(47 21) #\[)
                 #(tag-sym #(47 23) "x")
                 #(tag-cln #(47 25) #\:)
                 #(tag-sym #(47 27) "replace")
                 #(tag-sym #(47 35) "pred?")
                 #(tag-sym #(47 41) "proc")
                 #(tag-sym #(47 46) "xs")
                 #(tag-rbrk #(47 49) #\])
                 #(tag-sym #(49 1) "replace")
                 #(tag-kw #(49 9) "zero?")
                 #(tag-lmbd #(50 9) "\\")
                 #(tag-sym #(50 11) "x")
                 #(tag-from #(50 13) "->")
                 #(tag-sym #(50 16) "x")
                 #(tag-pls #(50 18) "+")
                 #(tag-num #(50 20) 1)
                 #(tag-lbrk #(51 9) #\[)
                 #(tag-num #(51 11) 0)
                 #(tag-num #(51 13) 1)
                 #(tag-num #(51 15) 2)
                 #(tag-num #(51 17) 3)
                 #(tag-num #(51 19) 0)
                 #(tag-rbrk #(51 21) #\])
                 #(tag-sym #(53 1) "replace")
                 #(tag-kw #(53 9) "odd?")
                 #(tag-lmbd #(54 9) "\\")
                 #(tag-sym #(54 11) "x")
                 #(tag-from #(54 13) "->")
                 #(tag-sym #(54 16) "x")
                 #(tag-mul #(54 18) "*")
                 #(tag-num #(54 20) 2)
                 #(tag-lbrk #(55 9) #\[)
                 #(tag-num #(55 11) 1)
                 #(tag-num #(55 13) 2)
                 #(tag-num #(55 15) 3)
                 #(tag-num #(55 17) 4)
                 #(tag-num #(55 19) 5)
                 #(tag-num #(55 21) 6)
                 #(tag-rbrk #(55 23) #\])
                 #(tag-sym #(57 1) "replace")
                 #(tag-lmbd #(57 9) "\\")
                 #(tag-sym #(57 11) "x")
                 #(tag-from #(57 13) "->")
                 #(tag-num #(57 16) 0)
                 #(tag-hghr #(57 18) ">")
                 #(tag-sym #(57 20) "x")
                 #(tag-sym #(58 9) "exp")
                 #(tag-lbrk #(59 9) #\[)
                 #(tag-num #(59 11) 0)
                 #(tag-num #(59 13) 1)
                 #(tag-num #(59 15) -1)
                 #(tag-num #(59 18) 2)
                 #(tag-num #(59 20) -2)
                 #(tag-num #(59 23) 3)
                 #(tag-num #(59 25) -3)
                 #(tag-rbrk #(59 27) #\])
                 #(tag-sym #(61 1) "replicate")
                 #(tag-sym #(61 11) "x")
                 #(tag-num #(61 13) 0)
                 #(tag-to #(61 15) "<-")
                 #(tag-lbrk #(61 18) #\[)
                 #(tag-rbrk #(61 19) #\])
                 #(tag-sym #(62 1) "replicate")
                 #(tag-sym #(62 11) "x")
                 #(tag-sym #(62 13) "n")
                 #(tag-to #(62 15) "<-")
                 #(tag-lbrk #(62 18) #\[)
                 #(tag-sym #(62 20) "x")
                 #(tag-cln #(62 22) #\:)
                 #(tag-sym #(62 24) "replicate")
                 #(tag-sym #(62 34) "x")
                 #(tag-lprn #(62 36) #\()
                 #(tag-sym #(62 38) "n")
                 #(tag-mns #(62 40) "-")
                 #(tag-num #(62 42) 1)
                 #(tag-rprn #(62 44) #\))
                 #(tag-rbrk #(62 46) #\])
                 #(tag-sym #(64 1) "replicate")
                 #(tag-str #(64 11) "\"a\"")
                 #(tag-num #(64 15) 5)
                 #(tag-sym #(65 1) "replicate")
                 #(tag-lbrk #(65 11) #\[)
                 #(tag-str #(65 13) "\"a\"")
                 #(tag-str #(65 17) "\"b\"")
                 #(tag-rbrk #(65 21) #\])
                 #(tag-num #(65 23) 3)
                 #(tag-sym #(66 1) "replicate")
                 #(tag-str #(66 11) "\"a\"")
                 #(tag-num #(66 15) 0)))

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
                                      'tag-str)))
               (and name
                    (let* ((name-token (get-simple-rule-token name))
                           (:apply (syntax-apply (+ (get-token-pos name-token)
                                                    1))))
                      (if :apply
                          (vector 'func-decl
                                  (list (vector 'func-name name-token)
                                        (vector 'argument (list :apply))))
                          name))))))

      (define (syntax-argument start-pos
                               args-can-be-exprs?
                               args-can-be-funcs?
                               args-can-be-cont?)
        (and (start-in? start-pos)
             (let* ((arg (or (and (not args-can-be-funcs?)
                                  (simple-rule start-pos
                                               'simple-argument
                                               'tag-sym
                                               'tag-kw
                                               'tag-str
                                               'tag-num))
                             (syntax-array start-pos args-can-be-exprs?)
                             (and args-can-be-cont?
                                  (syntax-arg-continuous start-pos
                                                         args-can-be-exprs?))
                             (and args-can-be-exprs? (syntax-apply start-pos))
                             (and args-can-be-exprs?
                                  (syntax-expr start-pos
                                               (not args-can-be-cont?)))))
                    (larg (if (list? arg) arg (list arg))))
               ;(print 'syntax-argument args-can-be-cont? arg)
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
