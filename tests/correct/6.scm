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
(define ++ append)

(define (replace . :args) (map-cond (((and ((lambda (:x) (= :x 3)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (:x) (and (list? :x) (null? :x))))) :args) ((lambda :args #t) :args)) (apply (lambda (pred? proc :g_) (let () (begin (quote ())))) :args)) ((and ((lambda (:x) (= :x 3)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (:x) (and (list? :x) (and-fold (cons (>= (length :x) 1) (map (lambda (:lambda-i :xi) ((eval-i :lambda-i) :xi)) (quote ((lambda (x) #t))) (give-first :x 1)))))))) :args) ((lambda :args #t) :args)) (apply (lambda (pred? proc :g__) (let ((x (car :g__)) (xs (cdr :g__))) (begin (map-cond (((pred? x) (append-s ((apply proc (list x))) ((replace pred? proc xs)))) (#t (append-s (x) ((replace pred? proc xs))))))))) :args)))))
(replace zero? (lambda (x) (+ x 1)) (list 0 1 2 3 0))
(replace odd? (lambda (x) (* x 2)) (list 1 2 3 4 5 6))
(replace (lambda (x) (> 0 x)) exp (list 0 1 -1 2 -2 3 -3))
