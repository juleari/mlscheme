(#%require "lex.scm")

(tokenize "ad")
(tokenize "1")
(tokenize "-a")
(tokenize "-a + b * x^2\n + dy")
(tokenize "as1fff + as^1232\newf * (1 - 5)/9 111ka")