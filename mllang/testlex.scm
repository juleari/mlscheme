(#%require "lex.scm")

#|(tokenize "ad")
(tokenize "1")
(tokenize "-a")
(tokenize "-a + b * x^2\n + dy")
(tokenize "as1fff + as^1232\newf * (1 - 5)/9 111ka")|#

(define port (open-input-file "/Users/juleari/Desktop/иу9/диплом/examples/sample1.sm"))

#|(define (read-words port)
  (define (add-word word words)
    (if (null? word)
        words
        (cons (list->string (reverse word)) words)))
  
  (define (helper word)
    (let ((ch (read-char port)))
      (if (eof-object? ch)
          (list->string (reverse word))
          (helper (cons ch word)))))
  
  (helper '()))

(tokenize (read-words port))|#

(define (trim? s)
  (or (eqv? s #\space)
      (eqv? s #\newline)
      (eqv? s #\tab)))

(define (tokenize-file file)
  (define (add-word word words)
    (if (null? word)
        (reverse words)
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
        (reverse tokens)
        (tokenize-words (cdr words) (cons (tokenize (car words)) tokens))))
  
  (tokenize-words (read-words '() '()) '()))

