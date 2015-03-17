#lang racket

(define level -1)
(define number "")
(define numberOfPushes 0)
(define atLeastOneSpace #f)

;I now have a fully functioning stack
(define stack '())

;chapter 15 slide 44 notes type declaration (int)
(define (peek) 
  (car stack)
); end peek

(define (push element) 
  (set! stack (cons element stack))
); end push

(define (pop)
  (let ((result (car stack)))
    (set! stack (cdr stack))
     result)
); end pop

(define (isArithmeticSymbol symbol) 
  (cond [(equal? symbol "+")#t]
        [(equal? symbol "-")#t]
        [(equal? symbol "/")#t]
        [(equal? symbol "*")#t]
        [else #f])
  )

(define (parseArithmetic arithmeticExpression state) 
  
  ;(displayln (string-length arithmeticExpression))
  (define isEmpty #f)
  (if (and (equal? (string-length arithmeticExpression) 0)(or (equal? state "endBracket")(equal? state "spaceAfterEndBracket"))) (set! isEmpty #t) '())
  
  ;check arithmetic
  (if (equal? isEmpty #f)  
      (let ([character (substring arithmeticExpression 0 1)]
            [restOfWord (substring arithmeticExpression 1 (string-length arithmeticExpression))])
       ; (display (string-append state "===> "))(displayln character)
        
        (cond [(string=? state "startBracket") 
               (cond [(string=? character " ")(parseArithmetic restOfWord "startBracket")]
                     [(string=? character "(")
                      (set! level (+ level 1))
                      (parseArithmetic restOfWord "arithmeticSymbol")
                      ]
                     [else (error (string-append "Incorrect syntax: " character))])
               ]
              [(string=? state "arithmeticSymbol")
               (cond [(string=? character " ")(parseArithmetic restOfWord "arithmeticSymbol")]
                     [(isArithmeticSymbol character)
                      (push character)
                      (set! numberOfPushes (+ numberOfPushes 1))
                      (parseArithmetic restOfWord "spaceAfterArithmeticSymbol")
                      ]
                     [else (error (string-append "Incorrect syntax: " character))])                                         
               ]                          
              [(string=? state "spaceAfterArithmeticSymbol")
               (cond [(and (string=? character " ")(equal? atLeastOneSpace #f))
                      (set! atLeastOneSpace #t)
                      (parseArithmetic restOfWord "spaceAfterArithmeticSymbol")
                      ]
                     [(string=? character " ")(parseArithmetic restOfWord "spaceAfterArithmeticSymbol")]
                     [(and (string->number character)(equal? atLeastOneSpace #t))
                      (set! atLeastOneSpace #f)
                      (parseArithmetic arithmeticExpression "number")
                      ]
                     [(string=? character "(")
                      (set! atLeastOneSpace #f)
                      (parseArithmetic arithmeticExpression "startBracket")
                      ]
                     [else (error (string-append "Incorrect syntax: " character))])
               ]
              [(string=? state "number")
               (cond [(string->number character)
                      (set! number (string-append number character))
                      (parseArithmetic restOfWord "number")
                      ]
                     [(string=? character " ")
                      (if (equal? level 0)(set! level (+ level 2))(set! level (+ level 1)))
                      (displayln (string-append "move " number " register-" (number->string (- level 1))))
                      (set! number "")
                      (parseArithmetic arithmeticExpression "spaceAfterNumber")
                      ]
                     [(string=? character "(")
                      (if (equal? level 0)(set! level (+ level 2))(set! level (+ level 1)))
                      (displayln (string-append "move " number " register-" (number->string (- level 1))))
                      (set! number "")
                      (parseArithmetic arithmeticExpression "startBracket")
                      ]
                     [(string=? character ")")
                      (if (equal? level 0)(set! level (+ level 2))(set! level (+ level 1)))
                      (displayln (string-append "move " number " register-" (number->string (- level 1))))
                      (set! number "")
                      (parseArithmetic arithmeticExpression "endBracket")
                      ]
                     [else (error (string-append "Incorrect syntax: " character))])                         
               ]
              [(string=? state "spaceAfterNumber")
               (cond [(string=? character " ")(parseArithmetic restOfWord "spaceAfterNumber")]
                     [(string=? character "(")(parseArithmetic arithmeticExpression "startBracket")]
                     [(string->number character)(parseArithmetic arithmeticExpression "number")]
                     [else (error (string-append "Incorrect syntax: " character))])
               ]
              [(string=? state "endBracket") 
               (cond [(string=? character ")")
                      (let ([localStack stack][element (pop)])
                        (let ([operator ""])
                          (case (string-append element "")
                             [("+")(set! operator "add")]
                             [("-")(set! operator "subtract")]
                             [("*")(set! operator "multiply")]
                             [("/")(set! operator "divide")]
                             )
                          (cond [(and (equal? (length stack) 0)(< numberOfPushes 2))(displayln (string-append operator " register-" (number->string (- (* (length localStack) 2) 1)) " register-" (number->string (* (length localStack) 2))))]
                                [(equal? (length stack) 0)(displayln (string-append operator " register-" (number->string (- (* (length localStack) 2) 1)) " register-" (number->string (+ (* (length localStack) 2) 1))))]
                                [else  (displayln (string-append operator " register-" (number->string (- (* (- numberOfPushes 1) 2) 1)) " register-" (number->string (* (- numberOfPushes 1) 2))))  ]
                          )
                         )
                       )
                      (set! level (- level 1))
                      (parseArithmetic restOfWord "spaceAfterEndBracket")
                      ]
                     [(string=? character " ")(parseArithmetic restOfWord "endBracket")]
                     [else (error (string-append "Incorrect syntax: " character))])
               ]
              [(string=? state "spaceAfterEndBracket") 
               (cond [(string=? character " ")(parseArithmetic restOfWord "spaceAfterEndBracket")]
                     [(string=? character "(")(parseArithmetic arithmeticExpression "startBracket")]
                     [(and(string=? character ")")(> level -1))(parseArithmetic arithmeticExpression "endBracket")]
                     [(string->number character)(parseArithmetic arithmeticExpression "number")]
                     [else (error (string-append "Incorrect syntax: " character))])
               ]
              [else (error "state error")]
              )
        )'())
  
  ); end parseArithmetic

;================Defined Functions Above==========================

(define (compile)
  (let/ec break
  (let loop ()
    (display "Input an arithmetic expression: ") 
    (define command (read-line))
    (cond [(string? command) 
           (parseArithmetic command "startBracket")
           (set! level -1)
           (set! stack '())
           (set! atLeastOneSpace #f)
           (set! numberOfPushes 0)
           ]
          [(string=? command "exit")  (break)]
          [else '()]) (loop)
    )
  )
  (displayln "exited successfully...")
); end main