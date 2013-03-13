#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 binary-ports)
             (ice-9 regex))

(define status #t)

(define (main args)
  (let loop ((args (cdr args))
             (first #t))
    (cond ((null? args)
           (newline)
           (quit status))
          (else
           (unless first
             (write-char #\space))
           (let ((arg (car args)))
             (echo arg)
             (loop (cdr args) #f))))))

(define-syntax regexp-case
  (lambda (stx)
    (syntax-case stx (=> else)
      ((_ (key ...) pos clauses ...)
       #'(let ((atom-key (key ...)))
           (regexp-case atom-key pos clauses ...)))
      ((_ key pos (else expr ...))
       #'(begin
           expr ...
           pos))
      ((_ key pos (regexp => func) next ...)
       (with-syntax ((compiled (make-regexp (syntax->datum #'regexp))))
         #'(let ((match (regexp-exec compiled key pos)))
             (if match
                 (begin
                   (func (match:substring match))
                   (match:end match))
                 (regexp-case key pos next ...)))))
      ((_ key pos (regexp expr ...) next ...)
       #'(regexp-case key pos (regexp => (lambda _ expr ...)) next ...)))))

(define (echo str)
  (define len (string-length str))
  (let loop ((pos 0))
    (when (< pos len)
      (let ((ch (string-ref str pos)))
        (cond ((eqv? ch #\\)
               (loop (regexp-case str (1+ pos)
                       ("^a" (write-char #\alarm))
                       ("^b" (write-char #\backspace))
                       ("^c" (quit status))
                       ("^f" (write-char #\page))
                       ("^n" (write-char #\newline))
                       ("^r" (write-char #\return))
                       ("^t" (write-char #\tab))
                       ("^v" (write-char #\vtab))
                       ("^\\\\" (write-char #\\))
                       ("^0([1-7][0-7]{0,2})?"
                        => (lambda (substr)
                             (define val (string->number substr 8))
                             (if (< val 256)
                                 (put-u8 (current-output-port) val)
                                 (set! status #f))))
                       (else (set! status #f)
                             (write-char #\\)))))
              (else
               (write-char ch)
               (loop (1+ pos))))))))
