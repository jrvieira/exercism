(import (rnrs))

(define (transpose matrix)
  (if (null? matrix)
    '()
    (apply map list matrix)))

