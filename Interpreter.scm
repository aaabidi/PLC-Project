(load "simpleParser.scm")

; runs the whole program e.g. (M "code.txt")
(define M
  (lambda (file)
    (M_state (parser file) '(()()))))
;You use '(() ()) for initial state You have a very bare-bones use of abstraction functions. You have c[ad]*r calls scattered everywhere.

; controls variable declaration and assignment, if, while, and return statements
(define M_state
  (lambda (lis state)
    (cond
      ((null? lis) state)
      ((list? (car lis)) (M_state (cdr lis) (M_state (car lis) state)))
      ((and (equal? (length lis) 3) (eq? 'var (car lis))) (M_stateAssign state (cadr lis) (M_boolean (mlist (caddr lis)) state)))
      ((and (equal? (length lis) 2) (eq? 'var (car lis))) (M_stateAssign state (cadr lis) null))
      ((eq? '= (car lis)) (M_assign (cadr lis) (caddr lis) state))
      ((eq? 'return (car lis)) (boolChecker(lookup 'r (M_stateAssign state 'r (M_boolean (mlist (cadr lis)) state)))))
      ((eq? 'if (car lis)) (M_if lis state))
      ((eq? 'while (car lis)) (M_while lis state))
      ; if M_state doesn't know how to deal with it, check if M_boolean does
      ((toBoolean? lis) (M_boolean lis state))
      ; if M_boolean can't evaluate the statment M_value must or it can't be evaluated
      (else (M_value lis state)))))

; computes logical boolean operations
(define M_boolean
  (lambda (lis state)
    (cond
      ((null? lis) (error "input not a statement" lis))
      ((list? (car lis)) (M_state (car lis) state))
      ((eq? '== (car lis)) (equal? (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '!= (car lis)) (not (equal? (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state))))
      ((eq? '<  (car lis)) (< (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '>  (car lis)) (> (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '<= (car lis)) (<= (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '>= (car lis)) (>= (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '&& (car lis)) (and (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '|| (car lis)) (or (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? '!  (car lis)) (not (M_state (mlist(cadr lis)) state)))
      ((eq? 'true (car lis)) #t)
      ((eq? 'false (car lis)) #f)
      ; if M_boolean can't evaluate a statement send it to M_state to run through all options
      (else (M_state lis state)))))

; finds the value of arithemtic expressions and variables
(define M_value
  (lambda (lis state)
    (cond
    ((null? lis) (error "input not a statement" lis))
    ((list? (car lis)) (M_state (car lis) state))
    ((number? (car lis)) (car lis))
    ((and (eq? '- (car lis)) (eq? 2 (length lis))) (* -1 (M_state (mlist (cadr lis)) state)))
    ((eq? '+ (car lis)) (+ (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
    ((eq? '- (car lis)) (- (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
    ((eq? '* (car lis)) (* (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
    ((eq? '/ (car lis)) (floor (/ (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state))))
    ((eq? '% (car lis)) (modulo (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
    (else (lookup (car lis) state)))))

; makes sure that variables that have not been declared cannot be assigned a value
(define M_assign
  (lambda (var expr state)
    (cond
      ((declared var state) (M_stateAssign state var (M_state (mlist expr) state)))
      (else (error "variable not declared" var)))))

; checks if a variable has been declared
(define declared
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (listVars state)) #f)
      ((eq? var (firstVar state)) #t)
      (else (declared var (nextInState state))))))


;finds the value of a variable given a state
(define lookup
  (lambda (name state)
    (cond
      ((null? (listVars state)) (error "Variable with specified name not found" name) )
      ((and (equal? name (firstVar state)) (eq? 'nul (firstVal state))) (error "Variable not assigned a value" name))
      ((equal? name (firstVar state)) (firstVal state))
      (else (lookup name (nextInState state))))))

;All the variables in the state
(define listVars
  (lambda (state)
    (car state)))

;first variable in the state
(define firstVar
  (lambda (state)
    (caar state)))
;value of the first variable
(define firstVal
  (lambda (state)
    (caadr state)))
    
;"cdr" of the state
(define nextInState
  (lambda (state)
    (cons (cdar state) (cons (cdr(cadr state)) '()))))


;adding a variable that has been assigned a value to a state
(define addToState
  (lambda (state a b)
    (cons (cons a (car state)) (cons (cons b (cadr state)) '()))))

;adding a variable that has been declared but not assigned
(define addUnassigned
  (lambda (state a)
    (cons (cons a (car state)) (cons (cons 'nul (cadr state)) '()))))

;removing a variable from the state
(define removeState
  (lambda (x state)
    (cond
      ((null? (car state)) state)
      ((eq? x (caar state)) (nextInState state))
      (else (addToState (removeState x (nextInState state)) (caar state) (caadr state))))))

; adds a variable to the state
(define M_stateAssign
  (lambda (state a b)
    (addToState (removeState a state) a b)))

; puts an atom in a list -> necessary to avoid errors
(define mlist
  (lambda (x)
    (cons x '())))


; checks if a statement can be evaluated by M_boolean
(define toBoolean?
  (lambda (lis)
    (cond
      ((null? lis) (error "input not a valid statement" lis))
      ((eq? '== (car lis)) #t)
      ((eq? '!= (car lis)) #t)
      ((eq? '< (car lis)) #t)
      ((eq? '> (car lis)) #t)
      ((eq? '<= (car lis)) #t)
      ((eq? '>= (car lis)) #t)
      ((eq? '&& (car lis)) #t)
      ((eq? '|| (car lis)) #t)
      ((eq? '! (car lis)) #t)
      ((eq? 'true (car lis)) #t)
      ((eq? 'false (car lis)) #t)
      (else #f))))

; controls while statements
(define M_while
  (lambda (lis state)
    (if (M_boolean (mlist (cadr lis)) state)
        (M_while lis (M_state (mlist (caddr lis)) state))
        (M_state '() state))))

;controls if statements
(define M_if
  (lambda (lis state)
    (cond
      ((M_boolean (cadr lis) state) (M_state (caddr lis) state))
      ((eq? 3 (length lis)) state)
      ((eq? 4 (length lis)) (M_state (cadddr lis) state)))))

; changed returned #t and #f to true and false
(define boolChecker
  (lambda (x)
    (cond
      ((eq? #t x) 'true)
      ((eq? #f x) 'false)
      (else x))))
