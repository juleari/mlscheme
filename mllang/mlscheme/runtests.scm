(define TAB 4)
(define TAB1 (- TAB 1))

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

(define-syntax ++
  (syntax-rules ()
    ((_ x)  (set! x (+ x 1)))
    ((_ x y)(set! x (+ x y)))))

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

(define (get-token-tag token)
  (vector-ref token T-TAG))

(define (get-token-coords token)
  (vector-ref token T-COORDS))

(define (get-token-value token)
  (vector-ref token T-VALUE))

(define (get-token-pos token)
  (let ((coords (get-token-coords token)))
    (vector-ref coords C-POSITION)))

(define (set-token-tag token tag)
  (vector-set! token T-TAG tag)
  token)

(define (set-token-coords token coords)
  (vector-set! token T-COORDS coords)
  token)

(define (set-token-value token value)
  (vector-set! token T-VALUE value)
  token)

(define (string-join xs sep)
  (apply string-append
         (cons (car xs)
               (map (lambda (x) (string-append sep x)) (cdr xs)))))

(define (xs-to-string open-b xs sep)
  (string-append open-b
                 (string-join (map toString xs) sep)
                 ")"))

(define (vect-to-string vect)
  (xs-to-string "#(" (vector->list vect) " "))

(define (list-to-string xs)
  (xs-to-string "(" xs "\n"))

(define (toString x)
  (cond ((not x)   "#f")
        ((eq? x #t) "#t")
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((list? x)   (list-to-string x))
        ((vector? x) (vect-to-string x))
        (else        x)))

(define kw '(scheme mod div if zero? eval))

(define (trim? s)
  (or (eqv? s #\space)
      (eqv? s #\newline)
      (eqv? s #\tab)))

(define (get-cut-tag word)
  (assq word '((#\( tag-lprn)
               (#\) tag-rprn)
               (#\[ tag-lbrk)
               (#\] tag-rbrk)
               (#\{ tag-lbrc)
               (#\} tag-rbrc)
               (#\. tag-dot)
               (#\: tag-cln))))

(define (sym-or-new-tag? word)
  (assq word '((#\\ tag-lmbd)
               (#\< tag-lwr)
               (#\% tag-mod)
               (#\^ tag-xor)
               (#\# tag-diez)
               (#\! tag-not))))

(define (sym-old-or-new-tag? word)
  (assq word '((#\- ((tag-sym  tag-sym)
                     (tag-lwr  tag-to)
                     (#f       tag-mns)))
               (#\+ ((tag-sym  tag-sym)
                     (tag-pls  tag-conc)
                     (#f       tag-pls)))
               (#\* ((tag-sym  tag-sym)
                     (tag-mul  tag-pow)
                     (#f       tag-mul)))
               (#\t ((tag-sym  tag-sym)
                     (tag-diez tag-true)
                     (#f       tag-mul)))
               (#\f ((tag-sym  tag-sym)
                     (tag-diez tag-fls)
                     (#f       tag-mul)))
               (#\/ ((tag-sym  tag-sym)
                     (tag-rem  tag-div)
                     (#f       tag-rem)))
               (#\> ((tag-sym  tag-sym)
                     (tag-mns  tag-from)
                     (#f       tag-hghr)))
               (#\| ((tag-sym  tag-sym)
                     (tag-bor  tag-or)
                     (#f       tag-bor)))
               (#&  ((tag-sym  tag-sym)
                     (tag-band tag-and)
                     (#f       tag-band)))
               (#\= ((tag-sym  tag-sym)
                     (tag-lwr  tag-leq)
                     (tag-hghr tag-heq)
                     (tag-not  tag-neq)
                     (#f       tag-eq))))))

(define tokenize
  (let ((line 1)
        (position 1)
        (iscomment #f))
    (lambda (word)
      
      (define (isnum? token)
        (let ((token-word (string->number (get-token-value token))))
          (and token-word
               (vector 'tag-num (get-token-coords token) token-word))))
      
      (define (isnumber?)
        (let ((token-word (string->number word))
              (ws         (string->list word))
              (coords     (vector line position)))
          (and token-word
               ;(x-not-in-list #\/ ws)
               (++ position (length ws))
               (list (vector 'tag-num coords token-word)))))
      
      (define (iskw?)
        (define (helper kw w)
          (and (not (null? kw))
               (or (eq? w (car kw))
                   (helper (cdr kw) w))))
        (let ((coords (vector line position)))
          (and (helper kw (string->symbol word))
               (++ position (length (string->list word)))
               (list (vector 'tag-kw coords word)))))
      
      (define (tag-sym? s token)
        (and (eqv? (get-token-tag token) 'tag-sym)
             (helper s token)))
      
      (define (new-tag? s token new-tag)
        (and (not (get-token-tag token))
             (set-token-tag token new-tag)
             (helper s token)))
      
      (define (sym-or-new-tag s token new-tag)
        (or (tag-sym? s token)
            (new-tag? s token new-tag)))
      
      (define (sym-old-or-new-tag s token old-new-tag)
        (or (tag-sym? s token)
            (let* ((old-tag      (get-token-tag token))
                   (new-tag-assq (assq old-tag old-new-tag))
                   (new-tag      (and new-tag-assq (cadr new-tag-assq))))
              (set-token-tag token new-tag)
              (helper s token))))
      
      (define (cons-tags tag tag-token token s)
        (let* ((tail (helper s token))
               (tag-tail (car tail)))
          #|(write tail)
          (newline)|#
          (set-token-tag tag-token tag)
          (if (vector? tag-tail)
              (if (get-token-tag tag-tail)
                  (cons tag-token tail)
                  (list tag-token))
              (if tag-tail
                  (cons tag-token (list tail))
                  (list tag-token)))))
      
      (define (cut-token-by-word token word)
        (define (helper ws counter)
          (let ((w (car ws))
                (s (cdr ws)))
            ;(print "a" token "b" (list->string (reverse counter)))
            (if (eqv? w word)
                (list (vector (get-token-tag token)
                              (get-token-coords token)
                              (list->string (reverse counter)))
                      (vector #f (vector line (- position 1)) word)
                      (vector #f (vector line position) (list->string s)))
                (helper s (cons w counter)))))
        (helper (string->list (get-token-value token)) '()))
      
      (define (cut-by-tag w s token cut-tag)
        (let* ((old-tag (get-token-tag token))
               (cuted-list (cut-token-by-word token w))
               (before     (car   cuted-list))
               (center     (cadr  cuted-list))
               (after      (caddr cuted-list))
               (nafter     (isnum? after))
               (consed     (if nafter
                               (list (set-token-tag center cut-tag) nafter)
                               (cons-tags cut-tag center after s))))
          (if old-tag
              (cons (or (isnum? before)
                        (set-token-tag before old-tag))
                    consed)
              consed)))
      
      (define (helper ws token)
        (if (null? ws)
            (list token)
            (let* ((w (car ws))
                   (s (cdr ws))
                   (sont  (sym-or-new-tag? w))
                   (ssont (sym-old-or-new-tag? w))
                   (ctag  (get-cut-tag w)))
              (++ position)
              (cond ((eqv? w #\newline) (and (set! iscomment #f)
                                             (set! position 1)
                                             (++ line)
                                             '()))
                    (iscomment          (and (++ position (length s))
                                             '()))
                    ((eqv? w #\tab)     (and (++ position TAB1)
                                             '()))
                    ((eqv? w #\space)   '())
                    ((eqv? w #\;)       (and (set! iscomment #t)
                                             '()))
                    (sont               (sym-or-new-tag s token (cadr sont)))
                    (ssont              (sym-old-or-new-tag s token (cadr ssont)))
                    (ctag               (cut-by-tag w s token (cadr ctag)))
                    (else               (helper s (set-token-tag token 'tag-sym)))))))
      
      (or (iskw?)
          (isnumber?)
          (helper (string->list word) (vector #f (vector line position) word))))))

(define (tokenize-file file)
  (define (add-word word words)
    (if (null? word)
        words
        (cons (list->string (reverse word)) words)))
  
  (define (read-words word words)
    (let ((ch (read-char file)))
      (or (and (eof-object? ch)
               (add-word word words))
          (and (trim? ch)
               (read-words '() (cons (string ch) (add-word word words))))
          (read-words (cons ch word) words))))
  
  (define (tokenize-words words tokens)
    (if (null? words)
        tokens
        (let ((t (tokenize (car words))))
          (tokenize-words (cdr words) (append tokens t)))))
  
  (tokenize-words (reverse (read-words '() '())) '()))

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

(define (run-test func args correct-ans)
  (equal? (apply func args)
          correct-ans))

(define input-ports-list  (map input-dir  (list /Users/juleari/Desktop/иу9/диплом/tests/in/1.sm)))
(define output-ports-list (map output-dir (list /Users/juleari/Desktop/иу9/диплом/tests/out/1.sm)))

(define tokens (list))

(map (lambda (input-port output-port)
       (set! tokens (tokenize-file input-port))
       (display (toString (syntax)) output-port)
       (close-output-port output-port))
     input-ports-list
     output-ports-list)