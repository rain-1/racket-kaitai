#lang racket

(provide process-file)

(require "indentation-parser.rkt")

(define (boolean x) (if x #t #f))

(define (process-line input)
  (let ((result (regexp-match #px"^( *)(- )?([[:graph:]]+) *: *([^\n]+)?\n"
                              input)))
    (if (and result (= (length result) 5))
        (let ((indent (string-length (bytes->string/utf-8 (list-ref result 1))))
              (dash? (boolean (list-ref result 2)))
              (identifier (bytes->string/utf-8 (list-ref result 3)))
              (content (if (list-ref result 4)
                           (bytes->string/utf-8 (list-ref result 4))
                           #f)))
          `(,indent ,identifier ,dash? ,content))
        #f)))

(define (process-lines input)
  (let loop ()
    (let ((result (process-line input)))
      (unless result
        (error "parse error inside process-lines"))
      (write result)
      (newline)
      (if (eof-object? (peek-char input))
          '()
          (cons result (loop))))))

(define (process-file input)
  (indentation-collect (process-lines input)))
