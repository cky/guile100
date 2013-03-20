#!/usr/bin/guile \
-e main -s
!#

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 binary-ports)
             (ice-9 format)
             (ice-9 getopt-long))

(define status #t)

(define (main args)
  (define opts (getopt-long args (get-getopt-options)))
  (when (assq 'unbuffered opts)
    (setvbuf (current-output-port) _IONBF))
  (let ((files (assq-ref opts '())))
    (if (null? files)
        (cat)
        (for-each (lambda (file)
                    (if (string=? file "-")
                        (cat)
                        (cat file)))
                  files))
    (catch 'system-error force-output write-error-handler)
    (quit status)))

(define cat
  (case-lambda
   (()
    (catch 'system-error cat-port (read-error-handler "stdin")))
   ((file)
    (catch 'system-error
           (cut call-with-input-file file cat-port)
           (read-error-handler file)))))

(define cat-port
  (case-lambda
   (()
    (cat-port (current-input-port)))
   ((in)
    (cat-port in (current-output-port)))
   ((in out)
    (define bv (get-bytevector-some in))
    (unless (eof-object? bv)
      (catch 'system-error (cut put-bytevector out bv) write-error-handler)
      (cat-port in out)))))

(define (read-error-handler label)
  (lambda args
    (perror label (system-error-errno args))
    (set! status #f)))

(define (write-error-handler . args)
  (perror "write error" (system-error-errno args))
  ;; Don't try to flush buffers at exit, since it'd obviously fail.
  (primitive-_exit 1))

(define (perror label errno)
  (format (current-error-port) "cat: ~a: ~a~%" label (strerror errno)))

(define (help _)
  (display "Usage: cat [OPTION]... [FILE]...\n")
  (display "Concatenate FILE(s), or standard input, to standard output.\n")
  (newline)
  (for-each (lambda (option)
              (format #t "  -~a, --~16a ~a~%"
                      (cadr (assq 'single-char (cdr option)))
                      (car option)
                      (cadr (assq 'description (cdr option)))))
            getopt-options)
  (quit))

(define (version _)
  (display "cat 0.1, for Guile100\n")
  (quit))

(define (get-getopt-options)
  ;; getopt-long doesn't like extraneous option properties, so filter out
  (map (lambda (option)
         (remove (lambda (prop)
                   (and (pair? prop) (eq? (car prop) 'description)))
                 option))
       getopt-options))

(define getopt-options
  `((unbuffered (single-char #\u) (value #f)
                (description "do not buffer standard output"))
    (help (single-char #\h) (value #f) (predicate ,help)
          (description "display this help and exit"))
    (version (single-char #\v) (value #f) (predicate ,version)
             (description "output version information and exit"))))
