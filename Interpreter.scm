
(require racket/trace)
; runs the whole program e.g. (M "code.txt")
(define M
  (lambda (file)
    (M_state (parser file) '((()())))))
;You use '(() ()) for initial state You have a very bare-bones use of abstraction functions. You have c[ad]*r calls scattered everywhere.

; controls variable declaration and assignment, if, while, and return statements
(define M_state
  (lambda (lis state)
    (cond
      ((null? lis) state)
      ((list? (car lis)) (M_state (cdr lis) (M_state (car lis) state)))
      ((and (equal? (length lis) 3) (eq? 'var (car lis))) (M_stateAssign state (cadr lis) (M_assign* lis state)))
      ((and (equal? (length lis) 2) (eq? 'var (car lis))) (M_stateAssign state (cadr lis) 'nul))
      ((eq? '= (car lis)) (M_assign (cadr lis) (M_valueSecond lis state) state))
      ((eq? 'return (car lis)) (M_stateReturn (cadr lis) state))
      ((eq? 'if (car lis)) (M_if lis state))
      ((eq? 'while (car lis)) (M_while lis state (lambda (v) v) (lambda (v) (M_while lis v break continue)))))))


(define M_assign*
  (lambda (lis state)
    (cond
      ((null? lis) error "input not a valid statement" lis)
      ((toBoolean? lis) (M_boolean (mlist(caddr lis)) state))
      (else (M_value (mlist(caddr lis)) state)))))
;(M_booleanSecond lis state)
;(boolChecker(lookup 'r (M_stateAssign state 'r (M_booleanFirst lis state))))
; computes logical boolean operations
(define M_boolean
  (lambda (lis state)
    (cond
      ((null? lis) (error "input not a statement" lis))
      ((list? (car lis)) (M_boolean (car lis) state))
      ((eq? '== (car lis)) (equal? (M_booleanFirst lis state) (M_booleanSecond lis state)))
      ((eq? '!= (car lis)) (not (equal? (M_booleanFirst lis state) (M_booleanSecond lis state))))
      ((eq? '<  (car lis)) (< (M_valueFirst lis state) (M_valueSecond lis state)))
      ((eq? '>  (car lis)) (> (M_valueFirst lis state) (M_valueSecond lis state)))
      ((eq? '<= (car lis)) (<= (M_valueFirst lis state) (M_valueSecond lis state)))
      ((eq? '>= (car lis)) (>= (M_valueFirst lis state) (M_valueSecond lis state)))
      ((eq? '&& (car lis)) (and (M_booleanFirst lis state) (M_booleanSecond lis state)))
      ((eq? '|| (car lis)) (or (M_booleanFirst lis state) (M_booleanSecond lis state)))
      ((eq? '!  (car lis)) (not (M_booleanFirst lis state)))
      ((eq? 'true (car lis)) #t)
      ((eq? 'false (car lis)) #f))))

;lis: (return (+ 4 5))


(define M_stateReturn
  (lambda (lis state)
    (cond
      ((number? lis) lis)
      ((not(list? lis)) (lookup lis state))
      ((and (toBoolean? lis) (eq? #t (lookup 'toReturn (M_stateAssign state 'toReturn (M_boolean lis state))))) 'true)
      ((and (toBoolean? lis) (eq? #f (lookup 'toReturn (M_stateAssign state 'toReturn (M_boolean lis state))))) 'false)
      (else (lookup 'toReturn (M_stateAssign state 'toReturn (M_value lis state)))))))
;Helper Method to Abstract M_boolean: gets the first thing to be evaluated in a comparison
(define M_booleanFirst
  (lambda lis state
    (M_boolean (mlist (cadr lis)) state)))

;Helper Method to Abstract M_boolean: gets the second thing to be evaluated in a comparison
(define M_booleanSecond
  (lambda lis state
    (M_boolean (mlist (caddr lis)) state)))

; finds the value of arithemtic expressions and variables
(define M_value
  (lambda (lis state)
    (cond
    ((null? lis) (error "input not a statement" lis))
    ((list? (car lis)) (M_value (car lis) state))
    ((number? (car lis)) (car lis))
    ((and (eq? '- (car lis)) (eq? 2 (length lis))) (* -1 (M_valueFirst lis state)))
    ((eq? '+ (car lis)) (+ (M_valueFirst lis state) (M_valueSecond lis state)))
    ((eq? '- (car lis)) (- ((M_valueFirst lis state) (M_valueSecond lis state))))
    ((eq? '* (car lis)) (* (M_valueFirst lis state) (M_valueSecond lis state)))
    ((eq? '/ (car lis)) (floor (/ (M_valueFirst lis state) (M_valueSecond lis state))))
    ((eq? '% (car lis)) (modulo (M_valueFirst lis state) (M_valueSecond lis state)))
    (else (lookup (car lis) state)))))

;Helper Method to Abstract M_value: gets the first thing to be evaluated in an operation
(define M_valueFirst
  (lambda (lis state)
    (M_value (mlist (cadr lis)) state)))

;Helper Method to Abstract M_value: gets the second thing to be evaluated in an operation
(define M_valueSecond
  (lambda (lis state)
    (M_value (mlist (caddr lis)) state)))

; makes sure that variables that have not been declared cannot be assigned a value
(define M_assign
  (lambda (var expr state)
    (cond
      ((and (eq? (toBoolean? (mlist expr)) #t) (declared var state)) (M_stateAssign state var (M_boolean (mlist expr) state)))
      ((and (eq? (toBoolean? (mlist expr)) #f) (declared var state)) (M_stateAssign state var (M_value (mlist expr) state)))
      (else (error "variable not declared" var)))))

; checks if a variable has been declared
(define declared
  (lambda (var state)
    (cond
      ((and (null? (listVars (topLayer state))) (null? (removeLayer state))) #f)
      ((null? (listVars (topLayer state))) (declared var (removeLayer state)))
      ((eq? var (firstVar (topLayer state))) #t)
      (else (declared var (cons (nextInState (topLayer state))(removeLayer state)))))))
;Test state
(define state
  '(((a b c)(1 2 3))((x y z)(4 5 6)))) 

;finds the value of a variable given a state
(define lookup
  (lambda (name state)
    (cond
      ((and (null? (listVars (topLayer state))) (null? (removeLayer state))) (error "Variable not declared" name))
      ((null? (listVars (topLayer state))) (lookup name (removeLayer state)))
      ((and (equal? name (firstVar (topLayer state))) (eq? 'nul (firstVal (topLayer state)))) (error "Variable not assigned a value" name))
      ((equal? name (firstVar (topLayer state))) (firstVal (topLayer state)))
      (else (lookup name (cons (nextInState (topLayer state))(removeLayer state)))))))

;Adding a layer onto the state
(define newLayer
  (lambda (state)
    (cons '(()()) state)))

;The top layer on a state
(define topLayer
  (lambda(state)
    (car state)))

;The state without the top layer
(define removeLayer
  (lambda(state)
    (cdr state)))


;All the variables in the top layer of the state
(define listVars
  (lambda (state)
     (car state)))

;All the values of the variables in the top layer of the state
(define listVals
  (lambda(state)
    (cadr state)))

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
    (cons (cdar  state) (cons (cdr(cadr state)) '()))))


;adding a variable that has been assigned a value to a state
(define addToState
  (lambda (state a b)
    (cons (cons (cons a (listVars (topLayer state))) (cons (cons b (listVals(topLayer state))) '()))(removeLayer state))))

;adding a variable that has been declared but not assigned
(define declare
  (lambda (state a)
    (cons (cons (cons a (listVars(topLayer state))) (cons (cons 'nul (listVals(topLayer state))) '()))(removeLayer state))))

;removing a variable from the state
(define removeState
  (lambda (x state)
    (cond
      ((and (null? (listVars(topLayer state))) (null? (removeLayer state))) state)
      ((null? (listVars(topLayer state))) (cons (topLayer state)(removeState x (removeLayer state))))
      ((eq? x (firstVar (topLayer state))) (cons (nextInState(topLayer state))(removeLayer state)))
      (else (addToState (removeState x (cons (nextInState(topLayer state))(removeLayer state))) (firstVar(topLayer state)) (firstVal(topLayer state)))))))

;update a declared variable
(define updateVar
  (lambda (x y state)
    (cond
      ((and (null? (listVars(topLayer state))) (null? (removeLayer state))) state)
      ((null? (listVars(topLayer state))) (cons (topLayer state)(updateVar x y (removeLayer state))))
      ((eq? x (firstVar (topLayer state))) (addToState (cons (nextInState (topLayer state)) (removeLayer state)) x y))
      (else (addToState (updateVar x y (cons (nextInState(topLayer state))(removeLayer state))) (firstVar(topLayer state)) (firstVal(topLayer state)))))))

; adds a variable to the state
(define M_stateAssign
  (lambda (state a b)
    (if (declared a state)
        (updateVar a b state)
        (addToState (removeState a state) a b))))

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
   (lambda (lis state break continue)
     (cond
       ((equal? 'break (car lis)) (break (M_while '() state)))
       ((equal? 'continue (car lis)) (continue (M_while '() state)))
       ((M_boolean (mlist (cadr lis) state)) (M_while lis (M_state (mlist (caddr lis) state))))
       (else (M_state '() state))
       )))

;controls if statements
(define M_if
  (lambda (lis state)
    (cond
      ((M_boolean (cadr lis) state) (M_state (caddr lis) state))
      ((eq? 3 (length lis)) state)
      ((eq? 4 (length lis)) (M_state (cadddr lis) state)))))


