#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 binary-ports))

(define status #t)

(define (main args)
  (setlocale LC_ALL "")
  (let loop ((args (cdr args))
             (first-arg #t))
    (cond ((null? args)
           (newline)
           (quit status))
          (else
           (unless first-arg
             (write-char #\space))
           (let ((arg (car args)))
             (call-with-input-string arg initial)
             (loop (cdr args) #f))))))

(define (echo ch port)
  (write-char ch)
  (initial port))

(define (initial port)
  (define ch (read-char port))
  (cond ((eqv? ch #\\)
         (backslash port))
        ((not (eof-object? ch))
         (echo ch port))))

(define (backslash port)
  (define ch (read-char port))
  (case ch
    ((#\a) (echo #\alarm port))
    ((#\b) (echo #\backspace port))
    ((#\c) (quit status))
    ((#\f) (echo #\page port))
    ((#\n) (echo #\newline port))
    ((#\r) (echo #\return port))
    ((#\t) (echo #\tab port))
    ((#\v) (echo #\vtab port))
    ((#\\) (echo #\\ port))
    ((#\0) (let ((next (peek-char port)))
             (if (and (assv next octal-digits)
                      (not (char=? next #\0)))
                 (octal port)
                 (echo #\nul port))))
    (else (set! status #f)
          (write-char #\\)
          (unless (eof-object? ch)
            (unread-char ch port)
            (initial port)))))

(define (octal port)
  (let loop ((value 0)
             (waiting 3))
    (cond ((zero? waiting)
           (if (< value 256)
               (put-u8 (current-output-port) value)
               (set! status #f))
           (initial port))
          (else (let ((ch (read-char port)))
                  (cond ((eof-object? ch)
                         (loop value 0))
                        ((assv ch octal-digits)
                         => (lambda (ass)
                              (loop (+ (* value 8) (cdr ass))
                                    (1- waiting))))
                        (else
                         (unread-char ch port)
                         (loop value 0))))))))

(define octal-digits
  '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3)
    (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7)))
