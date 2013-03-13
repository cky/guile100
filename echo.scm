#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 binary-ports))

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
             (call-with-input-string arg echo)
             (loop (cdr args) #f))))))

(define (echo port)
  (define ch (read-char port))
  (cond ((eqv? ch #\\)
         (backslash port))
        ((not (eof-object? ch))
         (write-char ch)
         (echo port))))

(define (backslash port)
  (define ch (read-char port))
  (case ch
    ((#\a #\b #\f #\n #\r #\t #\v #\\)
     (display (call-with-input-string (string #\" #\\ ch #\") read))
     (echo port))
    ((#\c) (quit status))
    ((#\0) (let ((next (peek-char port)))
             (if (and (char? next) (char<=? #\1 next #\7))
                 (octal port)
                 (begin
                   (write-char #\nul)
                   (echo port)))))
    (else (set! status #f)
          (write-char #\\)
          (unless (eof-object? ch)
            (unread-char ch port)
            (echo port)))))

(define (octal port)
  (let loop ((value 0)
             (waiting 3))
    (cond ((zero? waiting)
           (if (< value 256)
               (put-u8 (current-output-port) value)
               (set! status #f))
           (echo port))
          (else (let ((next (read-char port)))
                  (cond ((eof-object? next)
                         (loop value 0))
                        ((char<=? #\0 next #\7)
                         (loop (+ (* value 8) (- (char->integer next)
                                                 (char->integer #\0)))
                               (1- waiting)))
                        (else
                         (unread-char next port)
                         (loop value 0))))))))
