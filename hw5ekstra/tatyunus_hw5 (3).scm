(define get-operator (lambda (op-symbol env) 
 
 (cond 
 ((equal? op-symbol '+) +) 
 ((equal? op-symbol '-) -) 
 ((equal? op-symbol '*) *) 
 ((equal? op-symbol '/) /) 
 (else (get-value op-symbol env))
 )))

 (define define-stmt? (lambda (e)
 	;; An expression is a define statement
    ;; if it is a list, and the first element
    ;; is define, the second element is a symbol,
    ;; and the third element is an expression.
  (and (list? e) (equal? (car e) 'define) (symbol? (cadr e)) (= (length e) 3))))
 
(define if-stmt? (lambda (e)
	;; An expression is a if statement
    ;; if it is a list, and the first element
    ;; is if, and length of list 4
  (and (list? e) (equal? (car e) 'if) (= (length e) 4))))

(define letstr-stmt? (lambda (e)
	;; An expression is a letstr statement
    ;; if it is a list, and the first element
    ;; is letstar, and length of list 3
    
  (and (list? e) (equal? (car e) 'let*) (= (length e) 3))))
  
(define let-stmt? (lambda (e)
 	;; An expression is a let statement
    ;; if it is a list, and the first element
    ;; is let, and length of list 3
    
  (and (list? e) (equal? (car e) 'let) (= (length e) 3))))
  
(define operatorchecker? (lambda (o)
  (cond 
   ((eq? o '/) #t)
   ((eq? o '*) #t)
   ((eq? o '+) #t)
   ((eq? o '-) #t)
   (else #f)
  )))

 
(define get-value (lambda (var env)
    (cond
      ;; If the environment is empty, then we could not find 
      ;; a binding in this environment.
      ((null? env) (error "cs305: error"))

      ;; Check if the first pair is a binding for the
      ;; variable that we are looking for, and if it is
      ;; return the current value of the variable.
      ((equal? (caar env) var) (cdar env))

      ;; Otherwise, search in the remaning of the environment.
      (else (get-value var (cdr env))))))
	
(define extend-env (lambda (var val old-env)
      ;; Add the new variable binding to the 
      ;; beginning of the old environment.
      (cons (cons var val) old-env)))

(define repl (lambda (env)
  (let* (
         ; first print out some prompt
         (dummy1 (display "cs305> "))

         ; READ an expression
         (expr (read))

         ; Form the new environment
         (new-env (if (define-stmt? expr)
                      (extend-env (cadr expr) (s7-hw5extra (caddr expr) env) env)
                      env))

         ; EVALuate the expression read
         (val (if (define-stmt? expr)
                  (cadr expr)
                  (s7-hw5extra expr env)))

         ; PRINT the value evaluated together
         ; with a prompt as MIT interpreter does
         (dummy2 (display "cs305: "))
         (dummy3 (display val))

         ; get ready for the next prompt
         (dummy4 (newline))
         (dummy4 (newline)))
     (repl new-env))))
(define cond?
(lambda (e)
(and (list? e) (not(null? e)) (equal? (car e) 'cond) (cond_list? (cdr e)) (not(null? (cdr e))) (else_condition? (last_element (cdr e))) (other_else? (cdr e)))
)
)

(define cond_list?
(lambda (e)
(cond
((null? e) e)
((condition? (car e)) (cond_list? (cdr e)))
((else_condition? (car e)) (cond_list? (cdr e)))
(else (error "cs305: error "))
)
)
)

(define condition?
(lambda (e)
(and (list? e) (not(null? e)) (= (length e) 2))
)
)

(define else_condition?
(lambda (e)
(and (list? e) (not(null? e)) (= (length e) 2) (equal? (car e) 'else))
)
)

(define (last_element list)
(cond
((null? (cdr list)) (car list))
        (else (last_element (cdr list)))
)
)

(define other_else?
(lambda (e)
(cond
((else_condition? (car e)) 
(if (null? (cdr e))
e
(error "cs305: error")
)
)
(else (other_else? (cdr e)))
)
)
)

(define condcheck
(lambda (e env)
(if (eq? (car (car e)) 'else)
(car (cdr (car e)))
(if (eq? (s7-hw5extra (car (car e)) env) 0) 
(condcheck (cdr e) env)
(s7-hw5extra (car (cdr (car e))) env)
)
)
)
)

(define s7-hw5extra (lambda (e env) 

(cond 
  ;; If the input expression is a number, then
  ;; the value of the expression is that number.
 	((number? e) e)
	
	;; If the input expression is a symbol, then
    ;; get the current value binding of this variable.	
	((symbol? e) (get-value e env)) 
 	
	 ;; Otherwise, we must see a list.
	 ((not (list? e))
	 	 (error "cs305: error" ))
 
 ((if-stmt? e) (if (eq? (s7-hw5extra (cadr e) env) 0) 
                    ( s7-hw5extra (cadddr e) env) 
                    ( s7-hw5extra (caddr e) env)))
  ((let-stmt? e)
      (let ((name (map car  (cadr e)))
            (initials (map cadr (cadr e))))
        (let ((values (map (lambda (init) (s7-hw5extra init env)) initials)))
          (let ((new-env (append (map cons name values) env)))
            (s7-hw5extra (caddr e) new-env)))))
			
 
 ((letstr-stmt? e) (if (= (length (cadr e)) 1) 
		(let ((l (list 'let (cadr e) (caddr e))))    (let ((name (map car (cadr l))) (initials (map cadr (cadr l)))) 
																		(let ((values (map (lambda (init) (s7-hw5extra init env)) initials)))
																			(let ((new-env (append (map cons name values) env)))
																				(s7-hw5extra (caddr l) new-env)))))
		(let ((first (list 'let (list (caadr e)))) (rest (list 'let* (cdadr e) (caddr e)))) 
										(let ((l (append first (list rest)))) (let ((name (map car (cadr l))) (initials (map cadr (cadr l))))
														(let ((values (map (lambda (init) (s7-hw5extra init env)) initials)))
																		(let ((new-env (append (map cons name values) env)))
																			(s7-hw5extra (caddr l) new-env))))))))
																		
																			
((cond? e) (condcheck (cdr e) env))
 (else 
 
 (cond										
																					
	((operatorchecker? (car e)) (let ((operands (map s7-hw5extra (cdr e) (make-list (length (cdr e)) env))) (operator (get-operator (car e) env)))
		(cond 
		                                          ((and (equal? operator '+) (= (length operands) 0)) 0) 
												  ((and (equal? operator '*) (= (length operands) 0)) 1) 
												  ((and (or (equal? operator '-) (equal? operator '/)) (= (length operands) (or 0 1))) (error "cs305: error"))
												  (else (apply operator operands))
												  )))
	    (else (let* ((result (s7-hw5extra (list (get-value (car e) env) (cadr e)) env))) result))
	)
	
 ))))
 
(define cs305 (lambda () (repl '())))
