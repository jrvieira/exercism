(import (rnrs))

(define (balanced? string)
   (let loop [(x (string->list string)) (stack '())]
      (if (null? x)
         (null? stack)
         (case (car x)
            [( #\( ) (loop (cdr x) (cons #\) stack))]
            [( #\[ ) (loop (cdr x) (cons #\] stack))]
            [( #\{ ) (loop (cdr x) (cons #\} stack))]
            [( #\) #\] #\} ) (cond
               [(null? stack) #f]
               [(char=? (car x) (car stack)) (loop (cdr x) (cdr stack))]
               [else #f])]
            [else (loop (cdr x) stack)]))))

