(#(func-def
   (#(func-decl
      (#(func-name #(tag-sym #(2 1) "day-of-week"))
       #(argument
         (#(simple-argument #(tag-sym #(2 13) "day"))))
       #(argument
         (#(simple-argument #(tag-sym #(2 17) "month"))))
       #(argument
         (#(simple-argument #(tag-sym #(2 23) "year"))))))
    #(func-to #(tag-to #(2 28) "<-"))
    #(func-def
      (#(func-decl (#(func-name #(tag-sym #(3 5) "a"))))
       #(func-to #(tag-to #(3 7) "<-"))
       #(expr
         (#(tag-num #(3 11) 14)
          #(func-decl
            (#(func-name #(tag-sym #(3 14) "month"))))
          #(tag-mns #(3 12) "-")
          #(tag-num #(3 24) 12)
          #(tag-div #(3 21) "//")))))
    #(func-def
      (#(func-decl (#(func-name #(tag-sym #(4 5) "y"))))
       #(func-to #(tag-to #(4 7) "<-"))
       #(expr
         (#(func-decl
            (#(func-name #(tag-sym #(4 10) "year"))))
          #(func-decl
            (#(func-name #(tag-sym #(4 17) "a"))))
          #(tag-mns #(4 15) "-")))))
    #(func-def
      (#(func-decl (#(func-name #(tag-sym #(5 5) "m"))))
       #(func-to #(tag-to #(5 7) "<-"))
       #(expr
         (#(func-decl
            (#(func-name #(tag-sym #(5 10) "month"))))
          #(func-decl
            (#(func-name #(tag-sym #(5 19) "a"))))
          #(tag-num #(5 23) 12)
          #(tag-mul #(5 21) "*")
          #(tag-pls #(5 16) "+")
          #(tag-num #(5 29) 2)
          #(tag-mns #(5 27) "-")))))
    #(expr
      (#(tag-num #(7 6) 7000)
       #(func-decl (#(func-name #(tag-sym #(7 9) "day"))))
       #(tag-pls #(7 7) "+")
       #(func-decl (#(func-name #(tag-sym #(7 15) "y"))))
       #(tag-pls #(7 13) "+")
       #(func-decl (#(func-name #(tag-sym #(7 20) "y"))))
       #(tag-num #(7 25) 4)
       #(tag-div #(7 22) "//")
       #(tag-pls #(7 17) "+")
       #(func-decl (#(func-name #(tag-sym #(7 31) "y"))))
       #(tag-num #(7 36) 400)
       #(tag-div #(7 33) "//")
       #(tag-pls #(7 28) "+")
       #(tag-num #(7 44) 31)
       #(func-decl (#(func-name #(tag-sym #(7 47) "m"))))
       #(tag-mul #(7 45) "*")
       #(tag-num #(7 52) 12)
       #(tag-div #(7 49) "//")
       #(tag-pls #(7 41) "+")
       #(func-decl (#(func-name #(tag-sym #(8 12) "y"))))
       #(tag-num #(8 17) 100)
       #(tag-div #(8 14) "//")
       #(tag-mns #(8 9) "-")
       #(tag-num #(8 25) 7)
       #(tag-mod #(8 23) "%"))))))