;; examp
(define v '((("0->1"
              #(#((lambda (:x) (= :x 1))
                  ((lambda (:x) (and (list? :x) (null? :x))))
                  (())
                  (lambda :args #t))
                ()
                (('())))
              #(#((lambda (:x) (= :x 1))
                  ((lambda (:x)
                     (and (list? :x)
                          (:and-fold
                           (cons
                            (>= (length :x) 1)
                            (map
                             (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi))
                             '((lambda (x) (eqv? x 0)))
                             (give-first :x 1)))))))
                  ((:_ (continuous "xs")))
                  (lambda :args #t))
                ()
                (((:func-call append-s (:list (1)) (:list (:func-call "0->1" "xs"))))))
              #(#((lambda (:x) (= :x 1))
                  ((lambda (:x)
                     (and (list? :x)
                          (:and-fold
                           (cons
                            (>= (length :x) 1)
                            (map
                             (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi))
                             '((lambda (x) #t))
                             (give-first :x 1)))))))
                  (("x" (continuous "xs")))
                  (lambda :args #t))
                ()
                (((:func-call append-s (:list ("x")) (:list (:func-call "0->1" "xs"))))))))
            ((:func-call "0->1" (:qlist 0 2 7 0 5)) (:func-call "0->1" (:qlist 0 1 0 1 0)))))
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
;; end defs

;; lib
(define (// a b) (quotient a b))
(define (% a b) (remainder a b))

(define-syntax eval-i
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
  ;(print ((eval-i (car f-list)) (car a-list)))
  (or (null? f-list)
      (and ((eval-i (car f-list)) (car a-list))
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
  (x-in-xs? x "zero?" "null?" "odd?" "even?" "abs" "not" "round" "sqrt"))

(define (x-in-xs? x . xs)
  (and (not-null? xs)
       (or (equal? x (car xs))
           (apply x-in-xs? (cons x (cdr xs))))))

(define (is-str-op? s)
  (procedure? (eval-i (string->symbol s))))

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
                           ((eq? (car x) ':scheme)                (cadr x))
                           (else                                  (calc-rpn x))))
        (else x)))

;; new lib
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

;; new lib
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
  `(map-cond ,(map (lambda (type)
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
                   types)))

(define (generate-let-func name types inner)
  `(letrec ((,name (lambda :args ,(generate-func-types types))))
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

(define (check-expr-or-func type)
  ((eval-i (get-args-num-from-type type)) 0))

(define (generate-def def)
  (let* ((name (string->symbol (car def)))
         (types (cdr def)))
    (to-gen-file (if (:and-fold (map check-expr-or-func types))
                     (let ((type (car (reverse types))))
                       `(define ,name
                                ,(generate-let (get-defs-from-type type)
                                               type)))
                     `(define (,name . :args)
                        ,(generate-func-types types))))))

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
