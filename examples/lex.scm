(define (append-x x xs)
  (if x (cons x xs) xs))

(define (make-symbol a b)
  (string->symbol (if (symbol? a)
                      (string-append (symbol->string a) b)
                      b)))

(define (nl?  s) (eqv? s #\newline))
(define (pr?  s) (eqv? s #\space))
(define (tab? s) (eqv? s #\tab))

(define (trim? s)
  (or (eqv? s #\space)
      (eqv? s #\newline)
      (eqv? s #\tab)))

(define (op? s)
  (or (eqv? s #\+)
      (eqv? s #\-)
      (eqv? s #\*)
      (eqv? s #\/)
      (eqv? s #\^)))

(define (arren? s)
  (or (eqv? s #\()
      (eqv? s #\))))

(define (sym? x)
  (let ((ordx (char->integer x))
        (orda (char->integer #\a))
        (ordz (char->integer #\z))
        (ordb (char->integer #\A))
        (ordy (char->integer #\Z)))
    (or (and (>= ordx orda)
             (<= ordx ordz))
        (and (>= ordx ordb)
             (<= ordx ordy)))))

(define (digit? x)
  (let ((ordx (char->integer x))
        (ord0 (char->integer #\0))
        (ord9 (char->integer #\9)))
    (and (>= ordx ord0)
         (<= ordx ord9)
         (-  ordx ord0))))



(define tokenize
  (let ((line 1)
        (pos  0))
    (lambda (str)
      (define (create-token tag value)
        (vector line pos tag value))
      
      (define (change-token-tag token tag)
        (vector-set! token 2 tag)
        token)
      
      (define (change-token-value token value)
        (vector-set! token 3 value)
        token)
      
      (define (get-value token)
        (vector-ref token 3))
      
      (define (num str)
        (define npos pos)
        (define (helper xs int b digit real)
          (set! npos (+ 1 npos))
          ;(write npos)
          (if (null? xs)
              (list (list int npos))
              (let* ((x (car xs))
                     (s (cdr xs))
                     (d (digit? x))
                     (n (if real (/ b 10) 10))
                     (i (and d
                             (or (and real (+ (* d b) (get-value int)))
                                 (+ (* (get-value int) 10) d)))))
                (or (and (eq? x #\.) digit (not real) (null? s) (helper s int 0.1 digit #t))
                    (and d (helper s (change-token-value int i) n #t real))
                    (and (not digit) (set! pos (- pos 1)) (cons #f xs))
                    (and digit (cons (list int npos) xs))))))
        
        (set! pos (+ 1 pos))
        (helper str (create-token 'num 0) 1 #f #f))
      
      (define (helper str token tokens)
        (if (null? str)
            (reverse (append-x token tokens))
            (let* ((s (car str))
                   (t (cdr str))
                   (n (num str))
                   (i (car n))
                   (d (cdr n)))
              (cond ((nl?   s)     (set! line (+ 1 line))
                                   (set! pos 0)
                                   (helper t #f (append-x token tokens)))
                    ((pr?   s)     (set! pos (+ 1 pos))
                                   (helper t #f (append-x token tokens)))
                    ((tab?  s)     (set! pos (+ 4 pos))
                                   (helper t #f (append-x token tokens)))
                    ((and i token) (set! pos (- (cadr i) 1))
                                   (helper d
                                           (change-token-value token
                                                               (make-symbol (get-value token)
                                                                            (number->string
                                                                             (get-value (car i)))))
                                           tokens))
                    (i             (set! pos (- (cadr i) 1))
                                   (helper d #f (cons (car i) tokens)))
                    ((op?   s)     (set! pos (+ 1 pos))
                                   (helper t #f (cons (create-token 'op (string->symbol (string s)))
                                                      (append-x token tokens))))
                    ((arren? s)    (set! pos (+ 1 pos))
                                   (helper t #f (cons (create-token 'arren (string s))
                                                      (append-x token tokens))))
                    ((and (sym? s) token) (set! pos (+ 1 pos))
                                          (helper t
                                                  (change-token-value token
                                                                      (make-symbol (get-value token)
                                                                                   (string s)))
                                                  tokens))
                    ((sym?  s)     (set! pos (+ 1 pos))
                                   (helper t (create-token 'sym
                                                           (make-symbol token (string s)))
                                           tokens))
                    (else          #f)))))
      
      (helper (string->list str) #f '()))))

;; tests
(tokenize "ad")
(tokenize "1")
(tokenize "-a")
(tokenize "-a + b * x^2\n + dy")
(tokenize "as1fff + as^1232\newf * (1 - 5)/9 111ka")