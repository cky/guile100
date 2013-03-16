#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 binary-ports)
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
    (quit status)))

(define cat
  (case-lambda
   (()
    (catch 'system-error
           (lambda ()
             (cat-port (current-input-port)))
           (error-handler "stdin")))
   ((file)
    (catch 'system-error
           (lambda ()
             (call-with-input-file file cat-port))
           (error-handler file)))))

(define cat-port
  (case-lambda
   ((in)
    (cat-port in (current-output-port)))
   ((in out)
    (define byte (get-u8 in))
    (unless (eof-object? byte)
      (put-u8 out byte)
      (cat-port in out)))))

(define (error-handler label)
  (lambda (key subr message args data)
    (apply format (current-error-port)
           (string-append "~a: " message "~%") label args)
    (set! status #f)))

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
         (filter (lambda (prop)
                   (not (and (pair? prop) (eq? (car prop) 'description))))
                 option))
       getopt-options))

(define getopt-options
  `((unbuffered (single-char #\u) (value #f)
                (description "do not buffer standard output"))
    (help (single-char #\h) (value #f) (predicate ,help)
          (description "display this help and exit"))
    (version (single-char #\v) (value #f) (predicate ,version)
             (description "output version information and exit"))))
