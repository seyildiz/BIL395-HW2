(define vars '())

(define (lookup var alist)
  (let ((pair (assoc var alist)))
    (if pair
        (cdr pair)
        #f)))

(define (store var val alist)
  (cond ((assoc var alist)
         (cons (cons var val)
               (filter (lambda (pair) (not (eq? (car pair) var))) alist)))
        (else
         (cons (cons var val) alist))))

(define (tokenize str)
  (let* ((cleaned-str (string-append str " "))
         (tokens '()))
    (let loop ((chars (string->list cleaned-str))
               (current-token '()))
      (cond
        ((null? chars)
         (reverse
          (if (null? current-token)
              tokens
              (cons (list->string (reverse current-token)) tokens))))
        ((char-whitespace? (car chars))
         (if (null? current-token)
             (loop (cdr chars) current-token)
             (begin
               (set! tokens
                     (cons (list->string (reverse current-token)) tokens))
               (loop (cdr chars) '()))))
        ((memv (car chars) '(#\+ #\- #\* #\/ #\= #\( #\)))
         (if (null? current-token)
             (begin
               (set! tokens (cons (string (car chars)) tokens))
               (loop (cdr chars) '()))
             (begin
               (set! tokens
                     (cons (string (car chars))
                           (cons (list->string (reverse current-token))
                                 tokens)))
               (loop (cdr chars) '()))))
        (else
         (loop (cdr chars)
               (cons (car chars) current-token)))))))

(define (parse tokens)
  (define (parse-expr tokens output operators)
    (cond
      ((null? tokens)
       (if (null? operators)
           (if (null? (cdr output))
               (car output)
               (error "Invalid expression"))
           (parse-expr tokens
                       (cons (list (car operators)
                                   (cadr output)
                                   (car output))
                             (cddr output))
                       (cdr operators))))

      ((string->number (car tokens))
       (parse-expr (cdr tokens)
                   (cons (string->number (car tokens)) output)
                   operators))

      ((string=? (car tokens) "+")
       (parse-expr (cdr tokens) output (cons '+ operators)))

      ((string=? (car tokens) "-")
       (parse-expr (cdr tokens) output (cons '- operators)))

      ((string=? (car tokens) "*")
       (parse-expr (cdr tokens) output (cons '* operators)))

      ((string=? (car tokens) "/")
       (parse-expr (cdr tokens) output (cons '/ operators)))

      ((string=? (car tokens) "=")
       (parse-expr (cdr tokens) output (cons 'define operators)))

      ((string->symbol (car tokens))
       (parse-expr (cdr tokens)
                   (cons (string->symbol (car tokens)) output)
                   operators))

      (else
       (error "Unexpected token" (car tokens)))))

  (parse-expr tokens '() '()))

(define (evaluate expr)
  (cond
    ((number? expr)
     expr)
    ((symbol? expr)
     (let ((val (lookup expr vars)))
       (if val
           val
           (error "Undefined variable" expr))))
    ((list? expr)
     (case (car expr)
       ((+)
        (+ (evaluate (cadr expr)) (evaluate (caddr expr))))
       ((-)
        (- (evaluate (cadr expr)) (evaluate (caddr expr))))
       ((*)
        (* (evaluate (cadr expr)) (evaluate (caddr expr))))
       ((/)
        (/ (evaluate (cadr expr)) (evaluate (caddr expr))))
       ((define)
        (let ((var (cadr expr))
              (val (evaluate (caddr expr))))
          (set! vars (store var val vars))
          val))
       (else
        (error "Invalid operation" expr))))))

(define (calculator)
  (display "Calc> ")
  (let ((input (read-line)))
    (cond
      ((or (eof-object? input)
           (string=? input "exit"))
       (display "Bye!\n"))
      ((string=? input "")
       (calculator))
      (else
       (call-with-current-continuation
        (lambda (exit)
          (with-exception-handler
           (lambda (err)
             (display "Error: ")
             (display err)
             (newline)
             (exit #f))
           (lambda ()
             (let* ((tokens (tokenize input))
                    (parsed-expr (parse tokens)))
               (display (evaluate parsed-expr))
               (newline))))))
       (calculator)))))

(calculator)
