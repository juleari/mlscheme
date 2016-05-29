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

(define (newton . :args) (map-cond (((and ((lambda (:x) (= :x 4)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (x) #t) (lambda (x) #t))) :args) ((lambda :args #t) :args)) (apply (lambda (f df x e) (begin (map-cond (((< (abs (f x)) e) x) (#t (newton f df (- x (/ (apply f (list x)) (apply df (list x)))) e)))))) :args)))))
(define (golden . :args) (map-cond (((and ((lambda (:x) (= :x 4)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (x) #t) (lambda (x) #t))) :args) ((lambda :args #t) :args)) (apply (lambda (f x0 x1 e) (letrec ((fi (/ (+ (sqrt 5) 1) 2))) (letrec ((loop (lambda :args (map-cond (((and ((lambda (:x) (= :x 4)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (x) #t) (lambda (x) #t))) :args) ((lambda :args #t) :args)) (apply (lambda (f x0 x1 e) (letrec ((a (- x1 (/ (- x1 x0) fi)))) (letrec ((b (+ x0 (/ (- x1 x0) fi)))) (begin (map-cond (((>= (apply f (list a)) (apply f (list b))) (map-cond (((< (abs (- x1 a)) e) (/ (+ a x1) 2)) (#t (loop f a x1 e))))) (#t (map-cond (((< (abs (- b x0)) e) (/ (+ x0 b) 2)) (#t (loop f x0 b e))))))))))) :args))))))) (begin (loop f x0 x1 e))))) :args)))))
(round (newton (lambda (x) (** x 2)) (lambda (x) (* 2 x)) 1.0 1e-08))
(round (newton (lambda (x) (+ (+ (** x 2) (* 4 x)) 4)) (lambda (x) (+ (* 2 x) 4)) 5.0 1e-08))
(round (golden (lambda (x) (** x 2)) -2.0 2.0 1e-08))
(round (golden (lambda (x) (+ (+ (** x 2) (* 4 x)) 4)) -5.0 5.0 1e-06))
(+ 5 6)
(not 8)
