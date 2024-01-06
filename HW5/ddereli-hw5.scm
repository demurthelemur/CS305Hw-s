(define get-operator
  (lambda (op-symbol)
    (case op-symbol
      ((+) +)
      ((*) *)
      ((-) -)
      ((/) /)
      (else #f))))

(define get-value (lambda (var env original-env)
    (cond 
       ( (null? env) (display "cs305: ERROR\n\n") (repl original-env) )
       ( (eq? var (caar env)) (cdar env))
       ( else (get-value var (cdr env) original-env)))))

(define if-expr?
  (lambda (e)
    (and (list? e) (= (length e) 4) (eq? (car e) 'if))
  )
)

(define lambda-expr?
  (lambda (e env)
    (cond
      ((not (list? e)) #f)
      ((eq? (car e) 'lambda)
       (and (list? (cadr e)) (list? (caddr e))))
      ((list? (car e))
       (and (eq? (caar e) 'lambda) 
            (list? (cadar e)) 
            (list? (caddar e)) 
            (= (length (cdr e)) (length (cadar e)))))
      (else #f))))


(define (bind-elem? e)
  (cond
    ((not (list? e)) #f)
    ((null? e) #t)
    (else
      (and (list? (car e))
           (= 2 (length (car e)))
           (symbol? (caar e))
           (bind-elem? (cdr e))))))


(define (multi-bind? lst checklst)
  (cond
    ((null? lst) #t)
    ((member (caar lst) checklst) #f)
    (else (multi-bind? (cdr lst) (cons (caar lst) checklst)))))

(define let?
  (lambda (e)
    (and (list? e) (= (length e) 3) (eq? (car e) 'let) (list? (cadr e)) (bind-elem? (cadr e)) (multi-bind? (cadr e) '()))
  )
)

(define extend-env (lambda (var val old-env)
        (cons (cons var val) old-env)))

(define define-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol?(cadr e)))))

(define s6
  (lambda (e env)
    (cond
      ;; Handling numbers and symbols
      ((number? e) e)
      ((symbol? e)
       (cond
         ((get-operator e) "[PROCEDURE]")
         ((lambda-expr? (get-value e env env) env) "[PROCEDURE]")
         (else (get-value e env env))))

      ;; Error handling for non-list expressions
      ((not (list? e)) 
       (display "cs305: ERROR\n\n") 
       (repl env))

      ;; Handling 'if' expressions
      ((if-expr? e)
       (s6 (if (eq? (s6 (cadr e) env) 0) (cadddr e) (caddr e)) env))

      ;; Handling 'lambda' expressions
      ((lambda-expr? e env)
       (if (eq? (car e) 'lambda)
           e
           (let* ((param-bind (map cons (cadar e) (cdr e)))
                  (new-env (append param-bind env)))
             (s6 (caddar e) new-env))))

      ;; Handling 'let' expressions
      ((let? e)
       (let* ((bindings (cadr e))
              (body (caddr e))
              (evaluated-bindings (map (lambda (binding)
                                         (s6 (cadr binding) env))
                                       bindings))
              (new-env (append (map cons (map car bindings) evaluated-bindings) env)))
         (s6 body new-env)))

      ;; Handling expressions with operators
      ((get-operator (car e))
       (let ((operator (get-operator (car e)))
             (operands (map (lambda (operand) (s6 operand env)) (cdr e))))
         (apply operator operands)))

      ;; Handling symbol expressions with two elements
      ((and (symbol? (car e)) (= (length e) 2))
       (let ((list-eval (list (get-value (car e) env env) (s6 (cadr e) env))))
         (s6 list-eval env)))

      ;; Default error handling
      (else 
       (display "cs305: ERROR\n\n") 
       (repl env)))))

(define repl (lambda (env)
   (let* (
           (dummy1 (display "cs305> "))
           (expr (read))
           (new-env (if (define-expr? expr) 
                        (extend-env (cadr expr) (s6 (caddr expr) env) env)
                        env
                    ))
           (val (if (define-expr? expr)
                    (cadr expr)
                    (s6 expr env)
                ))
           (dummy2 (display "cs305: "))
           (dummy3 (display val))
           (dummy4 (newline))
           (dummy5 (newline))
          )
          (repl new-env))))

(define cs305
        (lambda () (repl '()))
)