(import (rnrs))

(define (balanced? string)
   (let loop [(x (string->list string)) (stack '())]
      (cond
         [(null? x) (null? stack)]
         [(char=? #\( (car x)) (loop (cdr x) (cons #\) stack))]
         [(char=? #\[ (car x)) (loop (cdr x) (cons #\] stack))]
         [(char=? #\{ (car x)) (loop (cdr x) (cons #\} stack))]
         [(member (car x) (list #\) #\] #\}))
            (cond
              [(null? stack) #f]
              [(char=? (car x) (car stack)) (loop (cdr x) (cdr stack))]
              [else #f])]
         [else (loop (cdr x) stack)])))

