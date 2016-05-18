(define-syntax eval-i
  (syntax-rules ()
    ((_ x) (eval x (interaction-environment)))))

(define-syntax map-cond
  (syntax-rules ()
    ((_ ((cond-1 value-1) ...)) (cond (cond-1 value-1) ...))))

(define-syntax append-s
  (syntax-rules ()
    ((_ (x ...) (y ...)) (append (list x ...) y ...))))

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

(define // quotient)
(define % remainder)
(define ** expt)
(define (day-of-week . :args) (map-cond (((and ((lambda (:x) (= :x 3)) (length :args)) (hash (quote ((lambda (x) #t) (lambda (x) #t) (lambda (x) #t))) :args)) (apply (lambda (day month year) (letrec ((a (// (- 14 month) 12))) (letrec ((y (- year a))) (letrec ((m (- (+ month (* a 12)) 2))) (begin (% (- (+ (+ (+ (+ (+ 7000 day) y) (// y 4)) (// y 400)) (// (* 31 m) 12)) (// y 100)) 7)))))) :args)))))
(day-of-week 17 5 2016)
(day-of-week 10 4 2016)
(day-of-week 29 3 2016)
(day-of-week 20 4 2016)
