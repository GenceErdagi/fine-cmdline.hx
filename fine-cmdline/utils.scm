(require (prefix-in helix.static. "helix/static.scm"))
(require "helix/misc.scm")

(provide get-history-file-path
         load-history-from-disk
         save-history-to-disk
         string-trim
         string-split-manual
         join-strings
         for-index)

(define (get-history-file-path)
  (let* ([init-path (helix.static.get-init-scm-path)]
         [dir (if (string=? init-path "") "" (parent-name init-path))])
    (if (string=? dir "")
        "fine-cmdline-history.scm"
        (string-append dir "/fine-cmdline-history.scm"))))

(define (load-history-from-disk)
  (let ([path (get-history-file-path)])
    (if (path-exists? path)
        (with-handler (lambda (err) '())
          (let ([content (read-port-to-string (open-input-file path))])
            (if (string=? content "")
                '()
                (eval-string content))))
        '())))

(define (save-history-to-disk history)
  (let ([path (get-history-file-path)])
    (with-handler (lambda (err) (set-error! (string-append "Failed to save history: " (to-string err))))
      (when (path-exists? path)
        (delete-file! path))
      (let ([port (open-output-file path)])
        (display "'(" port)
        (for-each (lambda (h) 
                    (write h port)
                    (display " " port)) 
                  history)
        (display ")" port)
        (close-output-port port)))))

(define (for-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (for-index func (cdr lst) (+ index 1)))))

(define (string-trim s)
  (define (trim-left chars)
    (cond [(null? chars) '()]
          [(char-whitespace? (car chars)) (trim-left (cdr chars))]
          [else chars]))
  (define (trim-right chars)
    (reverse (trim-left (reverse chars))))
  (list->string (trim-right (trim-left (string->list s)))))

(define (string-split-manual s)
  (define (helper chars current result in-quotes escaped)
    (cond
      [(null? chars)
       (if (null? current)
           (reverse result)
           (reverse (cons (list->string (reverse current)) result)))]
      [escaped
       (helper (cdr chars) (cons (car chars) current) result in-quotes #f)]
      [(char=? (car chars) #\\)
       (helper (cdr chars) current result in-quotes #t)]
      [(char=? (car chars) #\")
       (helper (cdr chars) current result (not in-quotes) #f)]
      [(and (char=? (car chars) #\space) (not in-quotes))
       (if (null? current)
           (helper (cdr chars) '() result #f #f)
           (helper (cdr chars) '() (cons (list->string (reverse current)) result) #f #f))]
      [else
       (helper (cdr chars) (cons (car chars) current) result in-quotes #f)]))
  (helper (string->list (string-trim s)) '() '() #f #f))

(define (join-strings lst sep)
  (if (null? lst)
      ""
      (if (null? (cdr lst))
          (car lst)
          (string-append (car lst) sep (join-strings (cdr lst) sep)))))
