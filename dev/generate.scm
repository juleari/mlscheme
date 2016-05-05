;; examp
(define v
  '((("fub"
      #(#((lambda (:x) (= :x 1))
          ((lambda (:x)
             (and (list? :x)
                  (and-fold
                   (cons
                    (>= (length :x) 1)
                    (map
                     (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi))
                     ((lambda (x) (eqv? x 0)) (lambda :x #t))
                     (reverse (cdr (reverse :x)))))))))
          ((:_ (continuous "xs"))))
        ()
        (((append '(1) '(("fub" "xs"))))))
      #(#((lambda (:x) (= :x 1))
          ((lambda (:x)
             (and (list? :x)
                  (and-fold
                   (cons
                    (>= (length :x) 1)
                    (map
                     (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi))
                     ((lambda (x) #t) (lambda :x #t))
                     (reverse (cdr (reverse :x)))))))))
          (("x" (continuous "xs"))))
        ()
        (((append '("x") '(("fub" "xs"))))))
      #(#((lambda (:x) (= :x 1))
          ((lambda (:x) (and (list? :x) (null? :x))))
          (()))
        ()
        (('())))))
    ()))
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

(define (get-args-check-from-type type)
  (vector-ref (get-args-from-type type) TYPE-ARGS-CHECK))

(define (get-defs-from-type type)
  (vector-ref type TYPE-DEFS))

(define (get-exprs-from-type type)
  (vector-ref type TYPE-EXPRS))

(define get-new-name
  (let ((name ":g"))
    (lambda ()
      (set! name (string-append name "_"))
      (string->symbol name))))

(define (to-sym xs)
  (map (lambda (x)
         (if (string? x)
             (string->symbol x)
             (get-new-name)))
       xs))

(define (hash f-list a-list)
  (or (null? f-list)
      (and ((eval-i (car f-list)) (car a-list))
           (hash (cdr f-list) (cdr a-list)))))

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
  (x-in-xs? t "+" "-" "/" "%" "*" "//"))

(define (x-in-xs? x . xs)
  (and (not-null? xs)
       (or (eqv? x (car xs))
           (apply x-in-xs? (cons x (cdr xs))))))

(define (is-str-op? s)
  (procedure? (eval-i (string->symbol s))))

(define (toSymb x)
  (cond ((string? x) (string->symbol x))
        (else        x)))

;; end lib

(define (calc-rpn xs)
  (define (helper stack xs)
    (if (null? xs)
        (car stack)
        (let ((x (car xs))
              (s (cdr xs)))
          (if (number? x)
              (helper (cons x stack) s)
              (if (is-op? x)
                  (helper (cons `(,(string->symbol x) ,(cadr stack) ,(car stack))
                                (cddr stack))
                          s)
                  (if (list? x)
                      (helper (cons (map toSymb x) stack) s)
                      (helper (cons (string->symbol x) stack) s)))))))
  (helper '() xs))

(define (is-variable types)
  (and (eq? (length types) 1)
       (zero? (length (get-args-names-from-type (car types))))))

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
    (eval-i `(define (,name . :args)
               (cond ,(map (lambda (type)
                             `(and (and (,(get-args-num-from-type type) (length :args))
                                        (hash ',(get-args-check-from-type type) :args))
                                   (apply (lambda ,(to-sym (get-args-names-from-type type))
                                            ,(generate-let (get-defs-from-type type)
                                                           type))
                                          :args)))
                           types))))))

(define (generate-defs defs)
  (map generate-def defs))

(define (calc-expr expr)
  (eval-i (generate-expr (list expr))))

(define (calc-exprs exprs)
  (map calc-expr exprs))

(define (generate model)
  (let* ((defs (car model))
         (exprs (cadr model)))
    (generate-defs defs)
    (calc-exprs exprs)))