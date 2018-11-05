#lang racket

(provide indentation-collect)

;; originally posted to answer racket uses mailing list question [Converting bulleted list to s-expression?]

(define (indentation-collect inputs)
  (let-values (((h t) (collect^ inputs)))
    (if (null? t)
        (list h)
        (cons h (indentation-collect t)))))

(define (collect^ inputs)
  (let ((indent (caar inputs))
        (head (cdar inputs))
        (inputs (cdr inputs)))
    (n-collect indent head inputs)))

(define (n-collect n head stream)
  ;; n-collect will collect up all
  ;; subtrees off the stream whose
  ;; level is > N
  (let loop ((subtrees '())
             (stream stream))
    (if (or (null? stream)
            (<= (caar stream) n))
        (values (cons head (reverse subtrees)) stream)
        (let-values (((subtree stream) (collect^ stream)))
          (loop (cons subtree subtrees)
                stream)))))
