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

(define F-ARGS 0)
(define F-DEFS 1)
(define F-BODY 2)

(define ERROR_NO_FUNC_BODY   "no expression in body")
(define ERROR_NO_EOF         "no end of file")
(define ERROR_EXPR_ARENS     "arens not closed")
(define ERROR_EXPR           "expr is not correct")
(define ERROR_NO_IF_CONDS    "no conditions in if")
(define ERROR_NO_IF_ACT      "no expression in if")
(define ERR_AFTER_CONTINUOUS "uncorrect after continuous")
(define ERR_NO_CONTINUOUS    "no continuous")
(define ERR_INCORRECT_APPLY  "apply has no arguments")

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-ARGS-SIMILAR 3)
(define TYPE-DEFS 1)
(define TYPE-EXPRS 2)
(define TYPE-ARGS-NAMES 2)

(define ERROR_NUM_OF_ARGS "the expected number of arguments does not match the given number")
(define ERROR_UNDEFINED_VARIABLE "cannot reference undefined identifier")

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

(define (toSymb x)
  (cond ((string? x) (string->symbol x))
        (else        x)))

(define (x-in-xs? x . xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (apply x-in-xs? (cons x (cdr xs))))))

(define (make-map-cond if-conds)
  `(map-cond ,(map (lambda (if-cond)
                     (list (calc-rpn (car if-cond))
                           (calc-rpn (cadr if-cond))))
                   if-conds)))

(define (func-apply x)
  (cond ((string? x) (string->symbol x))
        ((list? x)   (cond ((or (null? x) (eq? (car x) 'quote))   x)
                           ((x-in-xs? (car x) ':list ':func-call) (map func-apply (cdr x)))
                           ((eq? (car x) ':qlist)                 (cons 'list
                                                                        (map func-apply (cdr x))))
                           ((eq? (car x) ':lambda)                (list 'lambda
                                                                        (map func-apply (cadr x))
                                                                        (calc-rpn (caddr x))))
                           ((eq? (car x) ':cond)                  (make-map-cond (cdr x)))
                           (else                                  (calc-rpn x))))
        (else x)))

(define (get-true-expr)
  (vector 'expr (list #(tag-true #(0 0) #t))))

(define (get-token)
  (if (null? tokens)
      #(tag-end #(2 27) "eof")
      (let ((token (car tokens)))
        (set! tokens (cdr tokens))
        token)))

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
               (#\& ((tag-sym  tag-sym)
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
              (and (eq? new-tag 'tag-true)
                   (set-token-value token #t))
              (and (eq? new-tag 'tag-fls)
                   (set-token-value token #f))
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
                    (sont               (sym-or-new-tag s
                                                        token 
                                                        (cadr sont)))
                    (ssont              (sym-old-or-new-tag s
                                                            token
                                                            (cadr ssont)))
                    (ctag               (cut-by-tag w s token (cadr ctag)))
                    (else               (helper s (set-token-tag token 
                                                                 'tag-sym)))))))
      
      (or (iskw?)
          (isnumber?)
          (helper (string->list word)
                  (vector #f (vector line position) word))))))

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

(define-syntax cdns
  (syntax-rules ()
    ((_ x xs) (append xs (list x)))))

(define (filter pred? xs)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (pred? x)
            (cons x (filter pred? (cdr xs)))
            (filter pred? (cdr xs))))))

(define (get-rule-name rule)
  (vector-ref rule R-NAME))

(define (get-rule-terms rule)
  (vector-ref rule R-TERMS))

(define (get-token-from-simple-rule rule)
  (vector-ref rule R-TERMS))

(define (get-first-term rule)
  (car (get-rule-terms rule)))

(define (get-decl-from-def func-def)
  (get-first-term func-def))

(define (get-token-name-from-decl func-decl)
  (get-token-from-simple-rule (get-first-term func-decl)))

(define (get-args-from-decl func-decl)
  (cdr (get-rule-terms func-decl)))

(define (get-simple-arg-type arg-rule)
  (let ((token (get-token-from-simple-rule arg-rule)))
    (if (eq? 'tag-sym (get-token-tag token))
        `(lambda (x) #t)
        `(lambda (x) (eqv? x ,(get-token-value token))))))

(define (get-simple-arg-name arg-rule)
  (get-token-value (get-token-from-simple-rule arg-rule)))

(define (get-type-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument) (get-simple-arg-type first-term))
          ((eq? first-type 'array-simple)    (get-array-type first-term))
          ((eq? first-type 'continuous)      '()))))

(define (get-types-of-args func-args)
  (letrec ((types (map get-type-of-arg func-args))
           (helper (lambda (types)
                     (if (null? types)
                         '()
                         (if (null? (car types))
                             (helper (cdr types))
                             (cons (car types) (helper (cdr types))))))))
    (helper types)))

(define (get-name-of-arg arg-rule)
  (let* ((terms (get-rule-terms arg-rule))
         (first-term (car terms))
         (first-type (get-rule-name first-term)))
    (cond ((eq? first-type 'simple-argument)
           (get-simple-arg-name first-term)))))

(define (get-names-of-args func-args)
  (map get-name-of-arg func-args))

(define (get-num-of-args func-args)
  (if (null? func-args)
      `(lambda (x) (zero? x))
      (let* ((last (car (reverse func-args)))
             (terms (get-rule-terms last))
             (term (car terms))
             (type (get-rule-name term))
             (num  (length func-args)))  
          (if (eq? type 'continuous)
            `(lambda (:x) (>= :x ,(- num 1)))
            `(lambda (:x) (= :x ,num))))))

(define (add-func-in-model model name obj)
  (cons (list name obj) model))

(define (add-type-in-model model name val)
  (map (lambda (model-func)
         (let ((f-name (car model-func)))
           (if (equal? name f-name)
               (cdns val model-func)
               model-func)))
       model))

(define (find-in-model func-name model)
  (and (not-null? model)
       (let* ((car-model model)
              (in-model (assoc func-name car-model)))
         (or (and in-model
                  (append (cdr in-model)
                          (or (find-in-model func-name (cdr model))
                              '())))
             (and (find-in-params func-name car-model)
                  (cons (make-arg-type)
                        (or (find-in-model func-name (cdr model))
                            '())))
             (find-in-model func-name (cdr model))))))

(define (get-args-from-type type)
  (vector-ref type TYPE-ARGS))

(define (x-in-xs x xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (and (list? (car xs)) (x-in-xs x (car xs)))
           (x-in-xs x (cdr xs)))))

(define (find-in-params func-name model)
  (and (not-null? model)
       (:or-fold (map (lambda (type)
                        (x-in-xs func-name (get-args-names-from-type type)))
                      (cdar model)))))

(define (make-arg-type)
  (vector (vector `(lambda (:x) #t) (list) (list)) (list) (list)))

(define (make-alist xs)
  (define (helper xs alist)
    (if (null? xs)
        alist
        (helper (cdr xs) (cons (list (car xs) (make-arg-type)) alist))))
  (helper xs '()))

(define (remove-first n xs)
  (if (zero? n)
      xs
      (remove-first (- n 1) (cdr xs))))
  
(define (get-args-check-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-CHECK))

(define (get-similar-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-SIMILAR))

(define (get-defs-from-type type)
  (vector-ref type TYPE-DEFS))

(define (get-exprs-from-type type)
  (vector-ref type TYPE-EXPRS))

(define (to-sym xs)
  (map string->symbol xs))

(define (hash f-list a-list)
  (or (null? f-list)
      (and ((eval-i (car f-list)) (car a-list))
           (hash (cdr f-list) (cdr a-list)))))

(define (is-op? t)
  (x-in-xs? t "+" "-" "/" "%" "*" "//" ">" "<" ">=" "<=" "=" "!=" "++" "&&" "||"))

(define (op-in-xs? x . xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (apply op-in-xs? (cons x (cdr xs))))))

(define (is-str-op? s)
  (procedure? (eval-i (string->symbol s))))

;; new lib
(define (get-list-inner xs)
  (if (>= (length xs) 2)
      (reverse (cdr (reverse (cdr xs))))
      xs))

;; TRY!!!
(define (get-array-type arr-rule)
  (let* ((arr-terms (get-rule-terms arr-rule))
         (inner     (get-list-inner arr-terms))
         (l-inner   (length inner)))
    `(lambda (:x) (and (list? :x)
                       ,(if (null? inner)
                            `(null? :x)
                            (if (eq? 'continuous
                                     (get-rule-name (car (get-rule-terms (car (reverse inner))))))
                                (get-array-args-rules `(>= (length :x) ,(- l-inner 1))
                                                      (remove-last inner)
                                                      `(give-first :x ,(- l-inner 1)))
                                (get-array-args-rules `(= (length :x) ,l-inner)
                                                      inner
                                                      `:x)))))))

(define (get-simple-arg-value arg-rule)
  (get-token-value (get-token-from-simple-rule arg-rule)))

(define (get-args-num-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NUM))

(define (get-args-names-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NAMES))

(define (remove-last xs . ns)
  (let ((rxs (reverse xs))
        (n   (if (null? ns) 1 (car ns))))
    (reverse (remove-first n rxs))))

(define-syntax for
  (syntax-rules (in as)
    ((_ (item ...) in (items ...) proc ...)
     (for-each (lambda (item ...) (begin proc ...)) items ...))
    ((_ item in items proc ...)
     (for-each (lambda (item) (begin proc ...)) items))
    ((_ (items ...) as (item ...) . procs) (for (item ...) in (items ...) . procs))
    ((_ items as item . procs) (for item in items . procs))))

(define s-name
  (let* ((:s-name ':s))
    (lambda ()
      (set! :s-name
            (string->symbol (string-append (symbol->string :s-name) "_")))
      :s-name)))

(define (get-inner-length xs)
  (if (list? xs)
      (apply + (map get-inner-length xs))
      1))

(define (multi-list->vector xs)
  (define (helper xs)
    (if (list? xs)
        (apply append (map helper xs))
        (list xs)))
  (list->vector (helper xs)))

(define (vector->multi-list v xs)
  (define (helper cur-ml xs cur-ind)
    (if (null? xs)
        cur-ml
        (let ((cur-x (car xs)))
          (if (list? cur-x)
              (helper (append cur-ml
                              (list (helper '()
                                            cur-x
                                            cur-ind)))
                      (cdr xs)
                      (+ (get-inner-length cur-x) cur-ind))
              (helper (append cur-ml (list (vector-ref v cur-ind)))
                      (cdr xs)
                      (+ 1 cur-ind))))))
  (helper '() xs 0))

(define-syntax find-similar-in-args
  (syntax-rules ()
    ((_ args) (let* ((len (get-inner-length args))
                     (vec (multi-list->vector args))
                     (similar '())
                     (ind-source 0)
                     (ind-target 1)
                     (get-arg (lambda (ind) (vector-ref vec ind)))
                     (compare-args (lambda (ind1 ind2)
                                     (if (equal? (get-arg ind1)
                                                 (get-arg ind2))
                                         (begin (vector-set! vec ind2 (s-name))
                                                (list (list ind1 ind2)))
                                         (list)))))
                (letrec ((helper (lambda (ind-source ind-target similar)
                                   (or (and (eq? ind-source len) similar)
                                       (and (>= ind-target len)
                                            (helper (+ 1 ind-source) (+ 2 ind-source) similar))
                                       (helper ind-source
                                               (+ 1 ind-target)
                                               (append (compare-args ind-source
                                                                     ind-target)
                                                       similar))))))
                  (let ((res (helper ind-source ind-target '())))
                    (set! args (vector->multi-list vec args))
                    res))))))

(define-syntax make-similar-args-checks
  (syntax-rules ()
    ((_ args) (let ((:similar-pairs (find-similar-in-args args)))
                (if (not-null? :similar-pairs)
                    `(lambda :args
                       (let ((:v-args (multi-list->vector :args)))
                         (:and-fold-s ,(map (lambda (:similar-pair)
                                             `(equal? (vector-ref :v-args ,(car :similar-pair))
                                                      (vector-ref :v-args ,(cadr :similar-pair))))
                                           :similar-pairs))))
                    `(lambda :args #t))))))

(define (get-func-body f-inner)
  (vector-ref f-inner F-BODY))

(define (get-func-bodies model-func)
  (let ((f-inners (cdr model-func)))
    (map get-func-body f-inners)))

(define (make-rec-model func-name args-vector)
  (list func-name (vector args-vector (list) (list))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax while
  (syntax-rules ()
    ((_ cond? proc ...)
     (letrec ((iter (lambda () (if cond?
                                   (begin (begin proc ...)
                                          (iter))))))
       (iter)))))

(define get-new-name
  (let ((name ":g"))
    (lambda ()
      (set! name (string-append name "_"))
      (string->symbol name))))

(define (get-sym-name name)
  (if (string? name)
      (string->symbol name)
      (get-new-name)))

#| make-names-list
 | Создание списка имён.
 |  Строки преобразуются к символам.
 |  Для всех остальных типов генерируется новое имя
 | @param {list of arg-names} names список имён
 | @returns {list of names} список имён аргументов для lambda-функции
 |#
(define (make-names-list names)
  (map get-sym-name names))

#| Возвращает первые n элементов списка xs
 | @param {list} xs
 | @param {int} n
 | @returns {list}
 |#
(define (give-first xs n)
  (if (or (zero? n)
          (null? xs))
      '()
      (cons (car xs) (give-first (cdr xs) (- n 1)))))

(define (:and-fold xs)
  (or (null? xs)
      (and (car xs)
           (:and-fold (cdr xs)))))

(define (:or-fold xs)
  (and (not-null? xs)
       (or (car xs)
           (:or-fold (cdr xs)))))

(define (get-array-args-rules l-lambda inner x)
  `(:and-fold (cons ,l-lambda
                   (map (lambda (:lambda-i :xi)
                          ((eval-i :lambda-i) :xi))
                        ',(map (lambda (:i)
                                (get-type-of-arg :i))
                              inner)
                        ,x))))

(define (is-cont-name? name)
  ;(print name)
  (and (list? name)
       (not-null? name)
       (eq? (car name) 'continuous)))

(define (make-lambda-var-from-list xs)
  (if (null? xs)
      xs
      (let* ((r-xs (reverse xs))
             (last (car r-xs)))
        (if (is-cont-name? last)
            (if (> (length xs) 1)
                (append (map make-lambda-var (reverse (cddr xs)))
                        `(,(make-lambda-var (cadr r-xs)) . ,(cadr last)))
                (cadr last))
            (map get-sym-name xs)))))

(define (make-lambda-var elem)
  (if (list? elem)
      (make-lambda-var-from-list elem)
      (get-sym-name elem)))

;; только один уровень вложенности
(define (make-let-var-list names cor-names)
  #| Строит список определений для аргументов-списков
   | @param {list of arg-names} n-list Список имён аргументов
   | @param {symb} a-list Текущий остаток аргументов
   | @param {alist} l-list let-list (name value) Список let-определений
   | @returns {alist} l-list
   |#
  (define (helper n-list a-list l-list)
    (if (null? n-list)
        (reverse l-list)
        (let ((cur-name (car n-list)))
          (helper (cdr n-list)
                  `(cdr ,a-list)
                  (if (is-cont-name? cur-name)
                      (cons `(,(get-sym-name (cadr cur-name)) ,a-list) l-list)
                      (cons `(,(get-sym-name cur-name) (car ,a-list)) l-list))))))
  
  (apply append (map (lambda (name cor-name)
                       (if (list? name)
                           (helper name cor-name '())
                           '()))
                     names
                     cor-names)))

;; @returns (list lambda-var-list let-var-list)
(define (make-var-lists type)
  (let* ((names (get-args-names-from-type type))
         (has-let? (not-null? (filter (lambda (x)
                                        (and (list? x)
                                             (or (null? x)
                                                 (neq? (car x) 'continuous))))
                                      names))))

    (if has-let?
        (let ((cor-names (make-lambda-var-from-list names)))
          (list cor-names (make-let-var-list names cor-names)))
        (list (make-lambda-var-from-list names)))))

(define (get-args-for-check name type)
  (let ((names (get-args-names-from-type type)))
    (if (not-null? names)
        (let ((last (car (reverse names))))
          ;(print 'get-args-for-check last)
          (if (and (list? last)
                   (not-null? last)
                   (eq? (car last) 'continuous))
              `(give-first ,name ,(- (length names) 1))
              name))
        name)))

(define (is-apply? arg-values)
  (and (eq? 1 (length arg-values))
       (let ((val (car arg-values)))
         (and (list? val)
              (not-null? val)
              (eq? ':apply (car (reverse val)))))))

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
                    'tag-mod 'tag-rem  'tag-pow 'tag-not 'tag-conc))

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
                  ((x-in-xs? tag 'tag-pls 'tag-mns 'tag-conc)          8)
                  ((x-in-xs? tag 'tag-mul 'tag-div 'tag-mod 'tag-rem)  9)
                  ((x-in-xs? tag 'tag-pow)                            10)
                  ((x-in-xs? tag 'tag-not)                            11)
                  ((trigonometric)                                    12)
                  (else                                                0))))

        (define (unar? t)
          (x-in-xs? t 'tag-not))

        (define (try-get)
          (set! start-flag (> (get-token-pos token) start-pos))
          (and start-flag
               (let* ((tag  (get-token-tag token))
                      (op   (prior token))
                      (valid-op (and (> op 0)
                                     (or (not-null? stack)
                                         (not-null? out)
                                         (unar? tag))))
                      (to-out? (and (x-in-xs? tag 'tag-num 'tag-true 'tag-fls)
                                    (or (null? out)
                                        (> (prior (car (reverse out))) 0)
                                        (not-null? stack))))
                      (proc (syntax-func-declaration start-pos ARGS-CAN-BE-FUNCS))
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

(define semantic
  (let ((errors '())
        (f-name ':))
    (lambda (ast)

      (define (add-error error-type token)
        (set! errors
              (cons (vector 'error: token '- error-type)
                    errors))
        '())

      (define (add-error-rule rule token)
        (set! errors
              (cons (vector 'error: (get-token-coords token) '- 'expected `,rule)
                    errors))
        '())

      (define (print-errors)
        (if (null? errors)
            (display "SEMANTIC OK!")
            (begin (display "SEMANTIC ERRORS:")
                   (newline)
                   (apply print (reverse errors))))
        (newline)
        (newline))

      (define (semantic-get-new-var-name)
        (string->symbol (string-append (symbol->string f-name) "_")))

      (define (semantic-set-new-var-name)
        (set! f-name (semantic-get-new-var-name))
        f-name)

      (define (get-simple-arg-name arg-rule)
        (let ((val (get-simple-arg-value arg-rule)))
          (if (number? val)
              (semantic-set-new-var-name)
              val)))

      (define (get-array-name arr-rule)
        (let* ((terms (get-rule-terms arr-rule))
               (inner (get-list-inner terms)))
          (map get-name-of-arg inner)))

      ;; проверить, что там должны быть expr
      (define (get-continuous-name cont-rule)
        (let* ((terms (get-rule-terms cont-rule))
               ; terms : (list 'colon first-term)
               (first-term (cadr terms)))
          (list 'continuous (get-simple-arg-name first-term))))

      (define (get-name-of-arg arg-rule)
        (let* ((terms (get-rule-terms arg-rule))
               (first-term (car terms))
               (first-type (get-rule-name first-term)))
          (cond ((eq? first-type 'simple-argument) (get-simple-arg-name first-term))
                ((eq? first-type 'array-simple)    (get-array-name first-term))
                ((eq? first-type 'continuous)      (get-continuous-name first-term)))))

      (define (get-names-of-args func-args)
        (map get-name-of-arg func-args))

      (define (semantic-func-body func-def-terms names-of-args model)
        (let* ((b-list (cddr func-def-terms))
               (l-args (length names-of-args))
               (l-model (length model))
               (l-this (+ l-args l-model))
               (body (semantic-program b-list (append (make-alist names-of-args) model) '())))
          (list (remove-first l-this (car body)) (cdr body))))

      (define (semantic-func-def func-def-terms model)
        (let* ((func-decl (car func-def-terms))

               (func-name-token (get-token-name-from-decl func-decl))
               (func-name (get-token-value func-name-token))
               (in-model (find-in-model func-name model))

               (func-args (get-args-from-decl func-decl))
               (num-of-args (get-num-of-args func-args))
               (types-of-args (get-types-of-args func-args))
               (names-of-args (get-names-of-args func-args))
               (similar-args (make-similar-args-checks names-of-args))
               (args-vector (vector num-of-args types-of-args names-of-args similar-args))

               (this-model (cons (make-rec-model func-name args-vector) model))
               (body-list (semantic-func-body func-def-terms
                                              names-of-args
                                              this-model))
               (f-defs (car body-list))
               (f-exprs (cadr body-list))

               (func-val (vector args-vector f-defs f-exprs))
               (add-list (list model func-name func-val)))
          ;(print 'semantic-func-def add-list)
          (apply (if in-model
                     add-type-in-model
                     add-func-in-model)
                 add-list)))

      ;; проверять, что текущая функция используется
      ;; сначала просто проходим по определениям,
      ;; когда встречаем expr сохраняем его в список выражений для текущей области видимости
      ;; после того как все определения сформировались идём по выражениям и проверяем, что всё ок
      (define (semantic-func-call name-token args func-types)
        (let* ((name (get-token-value name-token))
               (arg-len (length args))
               (correct-types (filter (lambda (type)
                                        ((eval-i (get-args-num-from-type type)) arg-len))
                                      func-types)))
          ;(print 'semantic-func-call1 name arg-len func-types)
          (and (not-null? correct-types)
               (or  (and (zero? arg-len)
                         (or (and (> (length func-types) (length correct-types))
                                  (list ':func-call name))
                             name))
                    (let ((arg-values (map (lambda (arg)
                                             (get-arg-value (car (get-rule-terms arg))
                                                            (list (cons name func-types))))
                                           args)))
                      ;(print 'semantic-func-call2 name arg-values)
                      (if (is-apply? arg-values)
                          (list ':func-call "apply" name (caar arg-values))
                          (append (list ':func-call name) arg-values)))))))

      ;; надо связывать индексы в списке с функциями сравнения
      ;; для тех элементов, которые являются символами нужно хранить имена... ЖИЗНЬ БОЛЬ
      (define (semantic-var func-decl-terms model)
        (let* ((s-rule (car func-decl-terms))
               (name-token (get-token-from-simple-rule s-rule))
               (name (get-token-value name-token))
               (args (cdr func-decl-terms))
               (in-model (find-in-model name model)))
          ;(print 'semantic-var name args in-model model)
          (or (and in-model
                   (or (semantic-func-call name-token args in-model)
                       (add-error ERROR_NUM_OF_ARGS name-token)))
              (and (add-error ERROR_UNDEFINED_VARIABLE name-token)
                   func-decl-terms))))

      (define (get-continuous-expr cont-rule model)
        (let* ((terms   (get-rule-terms cont-rule))
               (expr    (cadr terms))
               (e-terms (get-rule-terms expr)))
          (semantic-expr e-terms model)))

      ;; нужно проверять, что simple-argument в model
      (define (argument-to-expr argument model)
        (let* ((terms       (get-rule-terms argument))
               (f-term      (car terms))
               (f-term-name (get-rule-name f-term)))
          ;(print 'argument-to-expr f-term-name)
          (cond ((eq? f-term-name 'simple-argument) (get-simple-arg-value f-term))
                ((eq? f-term-name 'continuous)      (get-continuous-expr f-term model))
                ((eq? f-term-name 'func-decl)       (car (semantic-expr terms model))))))

      ;; возможно, нужно переделать continuous
      (define (semantic-expr-arr arr-terms model)
        (let ((inner (get-list-inner arr-terms)))
          (if (null? inner)
              `'()
              (let* ((last (car (reverse inner)))
                     (list-elems (remove-last inner))
                     (terms       (get-rule-terms last))
                     (f-term      (car terms))
                     (f-term-name (get-rule-name f-term)))
                (if (eq? f-term-name 'continuous)
                    `(:func-call append-s ,(cons ':list (map (lambda (x) (argument-to-expr x model)) list-elems))
                                 ,(cons ':list (get-continuous-expr f-term model)))
                    `,(cons ':qlist (map (lambda (x) (argument-to-expr x model)) inner)))))))

      (define (get-arg-value arg-rule model)
        (let ((name (get-rule-name arg-rule)))
          ;(print 'get-arg-value arg-rule)
          (cond ((eq? name 'simple-argument) (get-simple-arg-value arg-rule))
                ((eq? name 'array-simple)    (semantic-expr-arr (get-rule-terms arg-rule) model))
                ((eq? name 'apply)           (cons (get-arg-value (car (get-rule-terms (cadr (get-rule-terms arg-rule))))
                                                                  model)
                                                   '(:apply)))
                ((eq? name 'expr)            (semantic-expr (get-rule-terms arg-rule)
                                                            (append (make-alist
                                                                     (vector-ref (get-args-from-type
                                                                                  (cadar model))
                                                                                 TYPE-ARGS-NAMES))
                                                                    model)))
                ((eq? name 'continuous)      (cons (get-arg-value (cadr (get-rule-terms arg-rule))
                                                                  model)
                                                   '(:apply))))))
      ;; if-cond: (#if-cond
      ;;           #expr
      ;;           #then
      ;;           #expr)
      (define (semantic-if-cond if-cond model)
        ;(print 'semantic-if-cond (cdddr if-cond))
        (list (semantic-expr (get-rule-terms (cadr if-cond)) model)
              (semantic-expr (get-rule-terms (car (cdddr if-cond))) model)))

      ;; terms: (#if-word
      ;;         ((#if-cond
      ;;           #expr
      ;;           #then
      ;;           #expr)
      ;;          (...)))
      (define (semantic-if-expr terms model)
        (cons ':cond (map (lambda (x) (semantic-if-cond x model))
                         (cadr terms))))

      (define (semantic-lambda func-def-terms model)
        ;(print 'semantic-lambda func-def-terms)
        (let* ((func-decl (car func-def-terms))

               (func-args (get-args-from-decl func-decl))
               (names-of-args (get-names-of-args func-args))

               (this-model (append (make-alist names-of-args) model))
               (body-list (semantic-func-body func-def-terms
                                              names-of-args
                                              this-model))

               (f-exprs (cadr body-list)))
          (list ':lambda names-of-args f-exprs)))

      (define (semantic-expr-elem elem model)
        (let ((type (get-rule-name elem)))
          (cond ((eq? type 'func-decl)    (semantic-var (get-rule-terms elem) model))
                ((eq? type 'array-simple) (semantic-expr-arr (get-rule-terms elem) model))
                ((eq? type 'if-expression)(semantic-if-expr (get-rule-terms elem) model))
                ((eq? type 'lambda-func)  (semantic-lambda (get-rule-terms elem) model))
                (else                     (get-token-value elem)))))

      (define (semantic-expr terms model)
        (map (lambda (x) (semantic-expr-elem x model)) terms))

      ;; need to make semantic-model-exprs
      (define (semantic-model-exprs model)
        ;(let* ((f-bodies (map get-func-bodies model))))
        model)

      ;; parse exprs after defs
      (define (semantic-program ast model exprs)
        (if (null? ast)
            (list (reverse model) exprs)
            (let* ((rule (car ast))
                   (name (get-rule-name rule))
                   (terms (get-rule-terms rule)))
              ;(print 'semantic-program name)
              (cond ((eq? name 'func-def)
                     (semantic-program (cdr ast)
                                       (semantic-func-def terms model)
                                       exprs))
                    ((eq? name 'expr)
                     (semantic-program (cdr ast)
                                       model
                                       (append exprs (semantic-expr terms model))))))))

      (let ((m (semantic-program ast '() '())))
        (and (print-errors) m)))))

(define gen-file (open-output-file "generated.scm"))

(define (prepare-gen-file)
  (let ((lib-funcs-file (open-input-file "genbase.sm")))
    (while (not (eof-object? (peek-char lib-funcs-file)))
           (display (read-char lib-funcs-file) gen-file))))

(prepare-gen-file)

(define (to-gen-file text)
  (display text gen-file)
  (newline gen-file))

(define (string-op->symbol x stack)
  (cond ((equal? x "!") (list (cons 'not stack)))
        (else           (cons (string->symbol x) stack))))

(define (calc-rpn xs)
  (define (helper stack xs)
    (if (null? xs)
        (car stack)
        (let ((x (car xs))
              (s (cdr xs)))
          (cond ((number? x) (helper (cons x stack) s))
                ((is-op? x)  (helper (cons `(,(string->symbol x) ,(cadr stack) ,(car stack))
                                           (cddr stack))
                                     s))
                ((list? x)   (helper (cons (func-apply x) stack) s))
                ((string? x) (helper (string-op->symbol x stack) s))
                ((x-in-xs? x #t #f) (helper (cons x stack) s))
                (else        (helper stack s))))))
  (helper '() xs))

(define (generate-let-var name exprs inner)
  `(letrec ((,name ,exprs))
     ,inner))

(define (generate-let defs exprs)
  (if (null? defs)
      exprs
      (let* ((def (car defs))
             (name (string->symbol (car def)))
             (types (cdr def)))
        (if (is-variable types)
            (generate-let-var name
                              (get-exprs-from-type (car type))
                              (generate-let (cdr defs) exprs))
            (generate-let-func name types (generate-let (cdr defs) exprs))))))

(define (is-variable types)
  (and (eq? (length types) 1)
       (zero? (length (get-args-names-from-type (car types))))))

(define (generate-let-var name exprs inner)
  `(letrec ((,name ,(calc-rpn (car exprs))))
     ,inner))

(define (generate-let defs type)
  (if (null? defs)
      (cons 'begin
            (map generate-expr (get-exprs-from-type type)))
      (let* ((def (car defs))
             (name (string->symbol (car def)))
             (types (cdr def)))
        (if (is-variable types)
            (generate-let-var name
                              (get-exprs-from-type (car types))
                              (generate-let (cdr defs) type))
            (generate-let-func name types (generate-let (cdr defs) type))))))

(define (generate-expr expr)
  (calc-rpn expr))

(define (generate-def def)
  (let* ((name (string->symbol (car def)))
         (types (cdr def)))
    (to-gen-file `(define (,name . :args)
                    (map-cond ,(map (lambda (type)
                                      (let* ((lambda-and-let (make-var-lists type))
                                             (lambda-list (car lambda-and-let))
                                             (lambda-let  (cdr lambda-and-let))
                                             (hash-args (get-args-for-check ':args type)))
                                        ;(print 'generate-def type)
                                        `((and (,(get-args-num-from-type type) (length :args))
                                               (hash ',(get-args-check-from-type type) ,hash-args)
                                               (,(get-similar-from-type type) :args))
                                          (apply (lambda ,lambda-list
                                                   ,(if (not-null? lambda-let)
                                                        `(let ,(car lambda-let)
                                                           ,(generate-let (get-defs-from-type type)
                                                                          type))
                                                        (generate-let (get-defs-from-type type)
                                                                      type)))
                                                 :args))))
                                    types))))))

(define (generate-defs defs)
  (map generate-def defs))

(define (calc-expr expr)
  (to-gen-file (generate-expr (list expr))))

(define (calc-exprs exprs)
  (map calc-expr exprs))

(define (generate model)
  (let* ((defs (car model))
         (exprs (cadr model)))
    (generate-defs defs)
    (calc-exprs exprs)
    (close-output-port gen-file)))

(define port (open-input-file "all.sm"))
(define tokens (tokenize-file port))
(define ast (syntax))
(define model (semantic ast))
(generate model)