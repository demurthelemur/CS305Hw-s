;Part 1 Two operator Calculator

(define twoOperatorCalculator
  (lambda (expr)
    (define (calculate lst)
      (if (null? lst)
          0
          (let ((first (car lst))
                (rest (cdr lst)))
            (cond
              ((number? first) (apply-ops first rest))
              (else (error "Invalid expression"))))))
    
    (define (apply-ops current lst)
      (if (null? lst)
          current
          (let ((op (car lst))
                (next (cadr lst))
                (rest (cddr lst)))
            (cond
              ((eq? op '+) (apply-ops (+ current next) rest))
              ((eq? op '-) (apply-ops (- current next) rest))
              (else (error "Invalid operator"))))))
    
    (calculate expr)))

;Part 2 Four operator Calculator

(define fourOperatorCalculator
  (lambda (expr)
    (define (process-expr lst)
      (if (null? lst)
          '()
          (let ((first (car lst))
                (rest (cdr lst)))
            (cond
              ((number? first) (process-ops first rest))
              (else (cons first (process-expr rest)))))))

    (define (process-ops current lst)
      (if (null? lst)
          (list current)
          (let ((op (car lst))
                (next (cadr lst))
                (rest (cddr lst)))
            (cond
              ((eq? op '*) (process-expr (cons (* current next) rest)))
              ((eq? op '/) (process-expr (cons (/ current next) rest)))
              (else (cons current (process-expr lst)))))))

    (process-expr expr)))

;Part 3 Nested Calculator

(define (calculatorNested expressionList)
  (define (processList exprList)
    (if (null? exprList)
        '()
        (let ((first (car exprList)) (rest (cdr exprList)))
          (if (list? first)
              (let ((sublistResult (twoOperatorCalculator (fourOperatorCalculator (processList first)))))
                (cons sublistResult (processList rest)))
              (cons first (processList rest))))))

  (processList expressionList))


;Part 4 Check Operators

(define checkOperators
  (lambda (expr)
    (define (is-valid-op? op)
      (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '/)))

    (define (check-list lst prev-was-num?)
      (cond
        ((null? lst) prev-was-num?)  
        ((list? (car lst)) (and (not prev-was-num?) (check-list (car lst) #f) (check-list (cdr lst) #t)))  
        ((number? (car lst)) (and (not prev-was-num?) (check-list (cdr lst) #t)))  
        ((is-valid-op? (car lst)) (and prev-was-num? (check-list (cdr lst) #f)))  
        (else #f)))  

    (if (list? expr) (check-list expr #f) #f)))  

;Part 5 Full Calculator

(define calculator
  (lambda (expr)
    (if (checkOperators expr)
        (let ((nested-result (calculatorNested expr)))
          (twoOperatorCalculator (fourOperatorCalculator nested-result)))
        #f)))  
 
