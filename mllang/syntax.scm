(define % quotient)
(define // remainder)
(define ** expt)

(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define C-LINE 0)
(define C-POSITION 1)

(define A-NAME 0)
(define A-RULES 1)

(define R-TOKEN 1)

(define ERROR_NO_FUNC_BODY   "no expression in body")
(define ERROR_NO_EOF         "no end of file")
(define ERROR_EXPR_ARENS     "arens not closed")
(define ERROR_EXPR           "expr is not correct")
(define ERROR_NO_IF_CONDS    "no conditions in if")
(define ERROR_NO_IF_ACT      "no expression in if")
(define ERR_AFTER_CONTINIOUS "uncorrect after continious")
(define ERR_NO_CONTINIOUS    "no continious")

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

(define tokens '(#(tag-sym #(2 1) "day-of-week")
                 #(tag-sym #(2 13) "day")
                 #(tag-sym #(2 17) "month")
                 #(tag-sym #(2 23) "year")
                 #(tag-to #(2 28) "<-")
                 #(tag-sym #(3 5) "a")
                 #(tag-to #(3 7) "<-")
                 #(tag-lprn #(3 10) #\()
                 #(tag-num #(3 11) 14)
                 #(tag-mns #(3 12) "-")
                 #(tag-sym #(3 14) "month")
                 #(tag-rprn #(3 19) #\))
                 #(tag-div #(3 21) "//")
                 #(tag-num #(3 24) 12)
                 #(tag-sym #(4 5) "y")
                 #(tag-to #(4 7) "<-")
                 #(tag-sym #(4 10) "year")
                 #(tag-mns #(4 15) "-")
                 #(tag-sym #(4 17) "a")
                 #(tag-sym #(5 5) "m")
                 #(tag-to #(5 7) "<-")
                 #(tag-sym #(5 10) "month")
                 #(tag-pls #(5 16) "+")
                 #(tag-lprn #(5 18) #\()
                 #(tag-sym #(5 19) "a")
                 #(tag-mul #(5 21) "*")
                 #(tag-num #(5 23) 12)
                 #(tag-rprn #(5 25) #\))
                 #(tag-mns #(5 27) "-")
                 #(tag-num #(5 29) 2)#||#
                 #(tag-lprn #(7 5) #\()
                 #(tag-num #(7 6) 7000)
                 #(tag-pls #(7 7) "+")
                 #(tag-sym #(7 9) "day")
                 #(tag-pls #(7 13) "+")
                 #(tag-sym #(7 15) "y")
                 #(tag-pls #(7 17) "+")
                 #(tag-lprn #(7 19) #\()
                 #(tag-sym #(7 20) "y")
                 #(tag-div #(7 22) "//")
                 #(tag-num #(7 25) 4)
                 #(tag-rprn #(7 26) #\))
                 #(tag-pls #(7 28) "+")
                 #(tag-lprn #(7 30) #\()
                 #(tag-sym #(7 31) "y")
                 #(tag-div #(7 33) "//")
                 #(tag-num #(7 36) 400)
                 #(tag-rprn #(7 39) #\))
                 #(tag-pls #(7 41) "+")
                 #(tag-lprn #(7 43) #\()
                 #(tag-num #(7 44) 31)
                 #(tag-mul #(7 45) "*")
                 #(tag-sym #(7 47) "m")
                 #(tag-div #(7 49) "//")
                 #(tag-num #(7 52) 12)
                 #(tag-rprn #(7 54) #\))
                 #(tag-mns #(8 9) "-")
                 #(tag-lprn #(8 11) #\()
                 #(tag-sym #(8 12) "y")
                 #(tag-div #(8 14) "//")
                 #(tag-num #(8 17) 100)
                 #(tag-rprn #(8 20) #\))
                 #(tag-rprn #(8 21) #\))
                 #(tag-mod #(8 23) "%")
                 #(tag-num #(8 25) 7)
                 #(tag-sym #(11 1) "count")
                 #(tag-sym #(11 7) "x")
                 #(tag-lbrk #(11 9) #\[)
                 #(tag-rbrk #(11 10) #\])
                 #(tag-to #(11 20) "<-")
                 #(tag-num #(11 23) 0)
                 #(tag-sym #(12 1) "count")
                 #(tag-sym #(12 7) "x")
                 #(tag-lbrk #(12 9) #\[)
                 #(tag-sym #(12 11) "x")
                 #(tag-cln #(12 13) #\:)
                 #(tag-sym #(12 15) "xs")
                 #(tag-rbrk #(12 18) #\])
                 #(tag-to #(12 20) "<-")
                 #(tag-num #(12 23) 1)
                 #(tag-pls #(12 25) "+")
                 #(tag-sym #(12 27) "count")
                 #(tag-sym #(12 33) "x")
                 #(tag-sym #(12 35) "xs")
                 #(tag-sym #(13 1) "count")
                 #(tag-sym #(13 7) "x")
                 #(tag-lbrk #(13 9) #\[)
                 #(tag-sym #(13 11) "y")
                 #(tag-cln #(13 13) #\:)
                 #(tag-sym #(13 15) "xs")
                 #(tag-rbrk #(13 18) #\])
                 #(tag-to #(13 20) "<-")
                 #(tag-sym #(13 23) "count")
                 #(tag-sym #(13 29) "x")
                 #(tag-sym #(13 31) "xs")
                 #(tag-sym #(15 1) "count")
                 #(tag-sym #(15 7) "'a")
                 #(tag-lbrk #(15 10) #\[)
                 #(tag-sym #(15 11) "'a")
                 #(tag-sym #(15 14) "'b")
                 #(tag-sym #(15 17) "'c")
                 #(tag-sym #(15 20) "'a")
                 #(tag-rbrk #(15 22) #\])
                 #(tag-sym #(17 1) "replace")
                 #(tag-sym #(17 9) "pred?")
                 #(tag-sym #(17 15) "proc")
                 #(tag-lbrk #(17 20) #\[)
                 #(tag-rbrk #(17 21) #\])
                 #(tag-to #(17 31) "<-")
                 #(tag-lbrk #(17 34) #\[)
                 #(tag-rbrk #(17 35) #\])
                 #(tag-sym #(18 1) "replace")
                 #(tag-sym #(18 9) "pred?")
                 #(tag-sym #(18 15) "proc")
                 #(tag-lbrk #(18 20) #\[)
                 #(tag-sym #(18 22) "x")
                 #(tag-cln #(18 24) #\:)
                 #(tag-sym #(18 26) "xs")
                 #(tag-rbrk #(18 29) #\])
                 #(tag-to #(18 31) "<-")
                 #(tag-kw #(19 5) "if")
                 #(tag-bor #(19 8) "|")
                 #(tag-sym #(19 10) "pred?")
                 #(tag-sym #(19 16) "x")
                 #(tag-from #(19 18) "->")
                 #(tag-lbrk #(19 21) #\[)
                 #(tag-sym #(19 23) "proc")
                 #(tag-sym #(19 28) "x")
                 #(tag-cln #(19 30) #\:)
                 #(tag-sym #(19 32) "replace")
                 #(tag-sym #(19 40) "pred?")
                 #(tag-sym #(19 46) "proc")
                 #(tag-sym #(19 51) "xs")
                 #(tag-rbrk #(19 54) #\])
                 #(tag-bor #(20 8) "|")
                 #(tag-from #(20 18) "->")
                 #(tag-lbrk #(20 21) #\[)
                 #(tag-sym #(20 28) "x")
                 #(tag-cln #(20 30) #\:)
                 #(tag-sym #(20 32) "replace")
                 #(tag-sym #(20 40) "pred?")
                 #(tag-sym #(20 46) "proc")
                 #(tag-sym #(20 51) "xs")
                 #(tag-rbrk #(20 54) #\])))

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
                   (apply print errors)))
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
      
      (define (syntax-rule* rule)
        (define (helper ast-list)
          (let ((ast-elem (rule)))
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
                                 second-rule
                                 error-name)
        (let ((first-rule (get-first-rule start-pos first-rule-expr)))
          (and first-rule
               (vector rule-name
                       (cons first-rule
                             (or (syntax-rule-type second-rule
                                                   (get-simple-start-pos first-rule))
                                 (add-error error-name)))))))
      
      (define (syntax-array-simple start-pos)
        (define ast-list '())
        (if (start-in? start-pos)
            (let ((first-rule (simple-rule 'open-braket 'tag-lbrk)))
              ;(print "syntax-array-simple" token ast-list first-rule)
              (and first-rule
                   (set! ast-list (cons first-rule
                                        (syntax-arguments (get-simple-start-pos first-rule))))
                   (or (let ((last-rule (simple-rule 'close-braket 'tag-rbrk)))
                         (and last-rule
                              (set! ast-list (append ast-list (list last-rule)))))
                       (add-error ERR_NO_CLOSE_BRK))
                   (list (vector 'array-simple ast-list))))
            ast-list))
      
      (define (syntax-continious-simple start-pos)
        (syntax-whole-rule 'continious
                           start-pos
                           `(,simple-rule 'colon 'tag-cln)
                           syntax-rule
                           `(,simple-rule 'list 'tag-sym)
                           ERR_NO_CONTINIOUS))
      
      (define (syntax-arg-continious start-pos)
        (and (start-in? start-pos)
             (let ((cont (syntax-continious-simple start-pos)))
               (and cont
                    (or (not (syntax-argument start-pos))
                        (add-error ERR_AFTER_CONTINIOUS))
                    (list cont)))))
      
      (define (syntax-argument start-pos)
        (and (start-in? start-pos)
             (let* ((arg (or (simple-rule 'simple-argument 'tag-num 'tag-sym)
                             (syntax-array-simple start-pos)
                             (syntax-arg-continious start-pos)))
                    (larg (if (list? arg) arg (list arg))))
               ;(print "syntax-argument" arg)
               (and arg (vector 'argument larg)))))
      
      (define (syntax-arguments start-pos)
        ;(print "syntax-arguments" start-pos)
        (define (helper ast-list)
          (let ((arg (syntax-argument start-pos)))
            (or (and arg (helper (cons arg ast-list)))
                (reverse ast-list))))
        (helper '()))
      
      (define (syntax-func-declaration start-pos)
        ;(print "syntax-func-declaration" start-pos)
        (and (start-in? start-pos)
             (let ((first-rule (simple-rule 'func-name 'tag-sym)))
               ;(print "syntax-func-declaration-first" first-rule)
               (and first-rule
                    (vector 'func-decl
                            (cons first-rule
                                  (syntax-rule? syntax-arguments
                                                (get-simple-start-pos first-rule))))))))
      
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
      
      (define (syntax-expr . args)
        ;(print "expr" token args)
        (let ((start-pos (car args)))
          (if (eq? (get-token-tag token) 'tag-end)
              (and (not-null? (cdr args))
                   (vector 'expr (cadr args)))
              (or (let ((arr (syntax-array-simple start-pos)))
                    ;(print "arr" arr)
                    (and (not-null? arr)
                         arr))
                  (syntax-if start-pos)
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
            (cond #|((x-in-xs? tag 'tag-bor)                             1)|#
              ((x-in-xs? tag 'tag-band)                            2)
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
                     (proc (syntax-func-declaration start-pos))
                     (flag #f))
                 ;(print "try-get" tag op proc)
                 (and (cond ((> op 0)             (op-to-out op)
                                                  (set! stack (cons token stack)))
                            (proc                 (set! flag #t)
                                                  (set! out (cons proc out)))
                            ((eqv? tag 'tag-num)  (set! out (cons token out)))
                            ((eqv? tag 'tag-lprn) (set! stack (cons token stack)))
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
                      ((op? cur) (from-stack-to-out cur op-before-laren-to-out p))
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
        (let ((func-decl (syntax-func-declaration start-pos)))
          (or (and func-decl
                   (let ((func-body (syntax-func-body (get-expr-start-pos func-decl))))
                     ;(print func-decl "syntax-program-func-body" func-body)
                     (or (and func-body
                              (vector 'func-def
                                      (cons func-decl func-body)))
                         (syntax-expr (get-expr-start-pos func-decl) func-decl))))
              (syntax-expr start-pos))))
      
      (let ((ast (syntax-rule+ syntax-program 0)))
        (and (neq? (get-token-tag token) 'tag-end)
             (add-error ERROR_NO_EOF))
        (print-errors)
        ast))))