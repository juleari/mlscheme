;; examp
(define v '((("0?"
              #f
              #(#((lambda (:x) (= :x 1)) ((lambda (x) (eqv? x 0))) (:_) (lambda :args #t))
                ()
                (((#t))))
              #(#((lambda (:x) (= :x 1)) ((lambda (x) #t)) ("x") (lambda :args #t))
                ()
                (((#f)))))
             ("my-gcd"
              :memo
              #(#((lambda (:x) (= :x 2))
                  ((lambda (x) #t) (lambda (x) #t))
                  ("a" "b")
                  (lambda :args #t))
                (("r"
                  #f
                  #(#((lambda (x) (zero? x)) () () (lambda :args #t)) () ((("a" "b" "%"))))))
                ((((:cond
                    ((("a" "b" "<")) (((:func-call "my-gcd" "b" "a"))))
                    ((((:func-call "0?" "r"))) (("b")))
                    (((#t)) (((:func-call "my-gcd" "b" "r"))))))))))
             ("my-lcm"
              #f
              #(#((lambda (:x) (= :x 2))
                  ((lambda (x) #t) (lambda (x) #t))
                  ("a" "b")
                  (lambda :args #t))
                ()
                ((("a" "b" "*" (:func-call "my-gcd" "a" "b") "/" "abs")))))
             ("prime?"
              (:memo :memo-prime?-fact)
              #(#((lambda (:x) (= :x 1)) ((lambda (x) #t)) ("n") (lambda :args #t))
                (("fact"
                  :memo-prime?-fact
                  #(#((lambda (:x) (= :x 1))
                      ((lambda (x) (eqv? x 0)))
                      (:__)
                      (lambda :args #t))
                    ()
                    (((1))))
                  #(#((lambda (:x) (= :x 1)) ((lambda (x) #t)) ("n") (lambda :args #t))
                    ()
                    ((("n" (:func-call "fact" (("n" 1 "-"))) "*"))))))
                ((((:func-call
                    "apply"
                    "0?"
                    (:qlist
                     (((:func-call "apply" "fact" (:qlist (("n" 1 "-"))))
                       1
                       "+"
                       "n"
                       "%"))))))))))
            (((:func-call "my-gcd" 3542 2464))
             ((:func-call "my-lcm" 3 4))
             ((:func-call "prime?" 11))
             ((:func-call "prime?" 12))
             ((:func-call "prime?" 3571)))))
;; end examp

;; defs
(define R-NAME 0)
(define R-TERMS 1)

(define T-TAG 0)
(define T-COORDS 1)
(define T-VALUE 2)

(define R-NAME 0)
(define R-TERMS 1)

(define TYPE-ARGS 0)
(define TYPE-ARGS-NUM 0)
(define TYPE-ARGS-CHECK 1)
(define TYPE-ARGS-NAMES 2)
(define TYPE-ARGS-SIMILAR 3)
(define TYPE-DEFS 1)
(define TYPE-EXPRS 2)
(define TYPE-ARGS-NAMES 2)

(define :ERROR_FUNC_CALL "the arguments does not match any types of this function")
;; end defs

;; lib
(define (// a b) (quotient a b))
(define (% a b) (remainder a b))

(define-syntax :eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define (filter pred? xs)
  (define (helper xs res)
    (if (null? xs)
        (reverse res)
        (helper (cdr xs)
                (let ((x (car xs)))
                  (if (pred? x)
                      (cons x res)
                      res)))))
  (helper xs '()))

(define (get-rule-name rule)
  (vector-ref rule R-NAME))

(define (get-rule-terms rule)
  (vector-ref rule R-TERMS))

(define (get-args-from-type type)
  (vector-ref type TYPE-ARGS))

(define (get-args-num-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NUM))

(define (get-args-names-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-NAMES))

(define (get-args-check-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-CHECK))

(define (get-similar-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-SIMILAR))

(define (get-defs-from-type type)
  (vector-ref type TYPE-DEFS))

(define (get-exprs-from-type type)
  (vector-ref type TYPE-EXPRS))

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

(define (hash f-list a-list)
  ;(print 'hash f-list a-list)
  ;(print ((:eval-i (car f-list)) (car a-list)))
  (or (null? f-list)
      (and ((:eval-i (car f-list)) (car a-list))
           (hash (cdr f-list) (cdr a-list)))))

(define-syntax neq?
  (syntax-rules ()
    ((_ x y) (not (eqv? x y)))))

(define-syntax not-null?
  (syntax-rules ()
    ((_ x) (not (null? x)))))

(define (print . xs)
  (or (and (not-null? xs)
           (display (car xs))
           (newline)
           (apply print (cdr xs)))
      (newline)))

(define (is-op? t)
  (x-in-xs? t "+" "-" "/" "%" "*" "//" ">" "<" ">=" "<=" "=" "!=" "++" "&&" "||"))

(define (is-uop? x)
  (x-in-xs? x "zero?" "null?" "odd?" "even?" "abs" "not" "round" "sqrt" "reverse"))

(define (x-in-xs? x . xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (apply x-in-xs? (cons x (cdr xs))))))

(define (is-str-op? s)
  (procedure? (:eval-i (string->symbol s))))

(define (make-map-cond if-conds)
  `(:map-cond ,(map (lambda (if-cond)
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
                           ((eq? (car x) ':scheme)                (cadr x))
                           (else                                  (calc-rpn x))))
        (else x)))

;; new lib
(define-syntax :map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax while
  (syntax-rules ()
    ((_ cond? proc ...)
     (letrec ((iter (lambda () (if cond?
                                   (begin (begin proc ...)
                                          (iter))))))
       (iter)))))

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
  ; (print 'and-fold xs)
  (or (null? xs)
      (and (car xs)
           (:and-fold (cdr xs)))))

(define (is-cont-name? name)
  (and (list? name)
       (not-null? name)
       (eq? (car name) ':continuous)))

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
                                                 (neq? (car x) ':continuous))))
                                      names))))
    (if has-let?
        (let ((cor-names (make-lambda-var-from-list names)))
          (list cor-names (make-let-var-list names cor-names)))
        (list (make-lambda-var-from-list names)))))

;; new lib
(define (get-args-for-check name type)
  (let ((names (get-args-names-from-type type)))
    (if (not-null? names)
        (let ((last (car (reverse names))))
          ;(print 'get-args-for-check last)
          (if (and (list? last)
                   (not-null? last)
                   (eq? (car last) ':continuous))
              `(give-first ,name ,(- (length names) 1))
              name))
        name)))
;; end lib
;(define gen-file (open-output-file "generated.scm"))
(define gen-file (current-output-port))

;; в рабочей версии вместо genbase.sm должен быть задаваемый в compiler.py путь
(define (prepare-gen-file)
  (let ((lib-funcs-file (open-input-file "genbase.sm")))
    (while (not (eof-object? (peek-char lib-funcs-file)))
           (display (read-char lib-funcs-file) gen-file))))

;(prepare-gen-file)

(define (to-gen-file text)
  (display text gen-file)
  (newline gen-file))

(define (string-op->symbol x stack)
  (cond ((equal? x "!") (list (cons 'not stack)))
        (else           (cons (string->symbol x) stack))))

(define (calc-rpn xs)
  (define (helper stack xs)
    ;(print 'calc-rpn xs stack)
    (if (null? xs)
        (car stack)
        (let ((x (car xs))
              (s (cdr xs)))
          (cond ((number? x) (helper (cons x stack) s))
                ((is-op? x)  (helper (cons `(,(string->symbol x) ,(cadr stack) ,(car stack))
                                           (cddr stack))
                                     s))
                ((is-uop? x) (helper (cons `(,(string->symbol x) ,(car stack))
                                           (cdr stack))
                                     s))
                ((list? x)   (helper (cons (func-apply x) stack) s))
                ((string? x) (helper (string-op->symbol x stack) s))
                ((x-in-xs? x #t #f) (helper (cons x stack) s))
                (else        (helper stack s))))))
  (helper '() xs))

(define (is-variable types)
  (and (eq? (length types) 1)
       (zero? (length (get-args-names-from-type (car types))))))

(define (generate-let-var name exprs inner)
  `(letrec ((,name ,(calc-rpn (car exprs))))
     ,inner))

(define (generate-func-types types)
  `(:map-cond ,(append
                (map (lambda (type)
                       (let* ((lambda-and-let (make-var-lists type))
                              (lambda-list (car lambda-and-let))
                              (lambda-let  (cdr lambda-and-let))
                              (hash-args (get-args-for-check ':args type)))
                         ;(print 'generate-def type)
                         `((and (,(get-args-num-from-type type) (length :args))
                                (:hash ',(get-args-check-from-type type) ,hash-args)
                                (,(get-similar-from-type type) :args))
                           (apply (lambda ,lambda-list
                                    ,(if (not-null? lambda-let)
                                         `(let ,(car lambda-let)
                                            ,(generate-let (get-defs-from-type type)
                                                           type))
                                         (generate-let (get-defs-from-type type)
                                                       type)))
                                  :args))))
                     types)
                `,(list (list 'else ':error-func-call)))))

(define (generate-let-func name memo-name types inner)
  (if memo-name
      `(letrec ((,name (lambda :args
                         (let ((:m (assoc :args ,memo-name)))
                           (if :m
                               (cadr :m)
                               (let ((:res ,(generate-func-types types)))
                                 (set! ,memo-name
                                       (cons (list :args :res) ,memo-name))
                                 :res))))))
         ,inner)
      `(letrec ((,name (lambda :args ,(generate-func-types types))))
         ,inner)))

(define (generate-let defs type)
  (if (null? defs)
      (let ((exprs (get-exprs-from-type type)))
        (if (> (length exprs) 1)
            (cons 'begin
                  (map generate-expr exprs))
            (generate-expr (car exprs))))
      (let* ((def (car defs))
             (name (string->symbol (car def)))
             (memo-and-types (cdr def))
             (is-memo? (car memo-and-types))
             (types (cdr memo-and-types)))
        ;(print name types)
        (if (is-variable types)
            (generate-let-var name
                              (get-exprs-from-type (car types))
                              (generate-let (cdr defs) type))
            (generate-let-func name is-memo? types (generate-let (cdr defs) type))))))

(define (generate-expr expr)
  (calc-rpn expr))

(define (check-expr-or-func type)
  ((:eval-i (get-args-num-from-type type)) 0))

(define (generate-memo-let memo-list)
  (if (list? memo-list)
      (map (lambda (name) (list name `(list))) memo-list)
      (list (list memo-list `(list)))))

(define (generate-memo-lambda memo-name inner)
  `(lambda :args
     (let ((:m (assoc :args ,memo-name)))
       (if :m
           (cadr :m)
           (let ((:res ,inner))
             (set! ,memo-name
                   (cons (list :args :res)
                         ,memo-name))
             :res)))))

(define (generate-def def)
  (let* ((name (string->symbol (car def)))
         (memo-and-types (cdr def))
         (is-memo? (car memo-and-types))
         (types (cdr memo-and-types)))
    ;(print name types)
    (to-gen-file (if (:and-fold (map check-expr-or-func types))
                     (let* ((type (car (reverse types)))
                            (exprs (get-exprs-from-type type)))
                       (if (not-null? exprs)  
                           `(define ,name
                              ,(generate-let (get-defs-from-type type)
                                             type))
                           ""))
                     (if is-memo?
                         `(define ,name
                            (let ,(generate-memo-let is-memo?)
                              ,(if (and (list? is-memo?)
                                        (not (x-in-xs? ':memo is-memo?)))
                                   `(lambda :args ,(generate-func-types types))
                                   (generate-memo-lambda ':memo
                                                         (generate-func-types types)))))
                         `(define (,name . :args)
                            ,(generate-func-types types)))))))

(define (generate-defs defs)
  (map generate-def defs))

(define (print-val val)
  (if (and (string? val)
           (> (string-length val) 8)
           (equal? (give-first (string->list val) 8) (string->list "(define ")))
      val
      `(print ,val)))

(define (calc-expr expr)
  (to-gen-file (print-val (generate-expr (list expr)))))

(define (calc-exprs exprs)
  (map calc-expr exprs))

(define (generate model)
  (let* ((defs (car model))
         (exprs (cadr model)))
    (generate-defs defs)
    (calc-exprs exprs)
    (close-output-port gen-file)))
