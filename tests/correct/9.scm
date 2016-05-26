(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax append-s
  (syntax-rules ()
    ((_ (x ...) (y ...)) (append (list x ...) y ...))))

(define-syntax :and-fold-s
  (syntax-rules ()
    ((_ ()) #t)
    ((_ (x)) x)
    ((_ (x y ...)) (and x (:and-fold-s (y ...))))))

(define (:and-fold xs)
  (or (null? xs)
      (and (car xs)
           (:and-fold (cdr xs)))))

(define (hash f-list a-list)
  (or (null? f-list)
      (and ((eval-i (car f-list)) (car a-list))
           (hash (cdr f-list) (cdr a-list)))))

(define (give-first xs n)
  (if (or (zero? n)
          (null? xs))
      '()
      (cons (car xs) (- n 1))))

(define (multi-list->vector xs)
  (define (helper xs)
    (if (list? xs)
        (apply append (map helper xs))
        (list xs)))
  (list->vector (helper xs)))

(define // quotient)
(define % remainder)
(define ** expt)
(define ++ append)
(define (&& x y) (and x y))

(define (and-fold . :args) (map-cond (((and ((lambda (x) (zero? x)) (length :args)) (hash (quote ()) :args) ((lambda :args #t) :args)) (apply (lambda () (begin #t)) :args)) ((and ((lambda (:x) (>= :x 1)) (length :args)) (hash (quote ((lambda (x) #t))) (give-first :args 1)) ((lambda :args #t) :args)) (apply (lambda (x . xs) (begin (&& x (apply and-fold xs)))) :args)))))
(and-fold #f #f #f)
(and-fold #f #f #t)
(and-fold #f #t #t)
(and-fold #t #t #t)
(and-fold)
