(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax append-s
  (syntax-rules ()
    ((_ (x ...) (y ...)) (append (list x ...) y ...))))

(define-syntax and-fold-s
  (syntax-rules ()
    ((_ ()) #t)
    ((_ (x)) x)
    ((_ (x y ...)) (and x (and-fold-s (y ...))))))

(define (and-fold xs)
  (or (null? xs)
      (and (car xs)
           (and-fold (cdr xs)))))

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
(define (fib . :args) (map-cond (((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (x) (eqv? x 0)))) :args) ((lambda :args #t) :args)) (apply (lambda (:g_) (begin 1)) :args)) ((and ((lambda (:x) (= :x 1)) (length :args)) (hash (quote ((lambda (x) #t))) :args) ((lambda :args #t) :args)) (apply (lambda (n) (begin (* n (fib (- n 1))))) :args)))))
(fib 5)
(fib 10)
