(load "simpleParser.scm")
(require racket/trace)


; runs the whole program e.g. (M "code.txt")
(define M
  (λ (file)
    (call/cc
     (λ (return)
       (M_state (parser file) '((()())) return (λ (v) v))))))
;You use '(() ()) for initial state You have a very bare-bones use of abstraction functions. You have c[ad]*r calls scattered everywhere.


; controls variable declaration and assignment, if, while, and return statements
(define M_state
  (λ (lis state return break)
    (cond
      ((null? lis) state)
      ((list? (car lis)) (M_state (cdr lis) (M_state (car lis) state return break) return break))
      ((and (equal? (length lis) 3) (eq? 'var (car lis))) (M_stateAssign state (cadr lis) (assignvalorbool lis state)))
      ((and (equal? (length lis) 2) (eq? 'var (car lis))) (M_stateAssign state (cadr lis) 'nul))
      ((eq? '= (car lis)) (M_stateAssign state (cadr lis) (assignvalorbool lis state)))
      ((eq? 'return (car lis)) (return (returnvalorbool lis state)))
      ((eq? 'if (car lis)) (M_if lis state return break))
      ((eq? 'while (car lis)) (M_while (cadr lis) (caddr lis) state return))
      ((eq? 'break (car lis)) (break state))
      ((eq? 'begin (car lis)) (M_stateBlock (cdr lis) state return break)))))


;decides whether to assign something a value or a boolean
(define assignvalorbool
  (λ (lis state)
    (cond
      ((null? lis) error "input not a valid statement" lis)
      ((toBoolean? (mlist (caddr lis))) (M_boolean (mlist(caddr lis)) state))
      (else (M_value (mlist(caddr lis)) state)))))


;decides whether to return a value or a boolean
(define returnvalorbool
  (λ (lis state)
    (cond
      ((null? lis) error "input not  a valid statement" lis)
      ((toBoolean? (cdr lis)) (M_boolean (cdr lis) state))
      (else (M_value (cdr lis) state)))))


;(M_booleanSecond lis state)
;(boolChecker(lookup 'r (M_stateAssign state 'r (M_booleanFirst lis state))))
; computes logical boolean operations
(define M_boolean
  (λ (lis state)
    (cond
      ((null? lis) (error "input not a statement" lis))
      ((list? (car lis)) (M_boolean (car lis) state))
      ((eq? '== (car lis)) (equal? (M_valueFirst lis state) (M_valueSecond lis state)))
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


(define M_stateBlock
  (λ (lis state return break)
    (removeLayer (M_state lis (newLayer state) return break))))


;Helper Method to Abstract M_boolean: gets the first thing to be evaluated in a comparison
(define M_booleanFirst
  (λ lis state
    (M_boolean (mlist (cadr lis)) state)))


;Helper Method to Abstract M_boolean: gets the second thing to be evaluated in a comparison
(define M_booleanSecond
  (λ lis state
    (M_boolean (mlist (caddr lis)) state)))


; finds the value of arithemtic expressions and variables
(define M_value
  (λ (lis state)
    (cond
    ((null? lis) (error "input not a statement" lis))
    ((list? (car lis)) (M_value (car lis) state))
    ((number? (car lis)) (car lis))
    ((and (eq? '- (car lis)) (eq? 2 (length lis))) (* -1 (M_valueFirst lis state)))
    ((eq? '+ (car lis)) (+ (M_valueFirst lis state) (M_valueSecond lis state)))
    ((eq? '- (car lis)) (- (M_valueFirst lis state) (M_valueSecond lis state)))
    ((eq? '* (car lis)) (* (M_valueFirst lis state) (M_valueSecond lis state)))
    ((eq? '/ (car lis)) (floor (/ (M_valueFirst lis state) (M_valueSecond lis state))))
    ((eq? '% (car lis)) (modulo (M_valueFirst lis state) (M_valueSecond lis state)))
    (else (lookup (car lis) state)))))


;Helper Method to Abstract M_value: gets the first thing to be evaluated in an operation
(define M_valueFirst
  (λ (lis state)
    (M_value (mlist (cadr lis)) state)))


;Helper Method to Abstract M_value: gets the second thing to be evaluated in an operation
(define M_valueSecond
  (λ (lis state)
    (M_value (mlist (caddr lis)) state)))


; makes sure that variables that have not been declared cannot be assigned a value
(define M_assign
  (λ (var expr state)
    (cond
      ((and (eq? (toBoolean? (mlist expr)) #t) (declared var state)) (M_stateAssign state var (M_boolean (mlist expr) state)))
      ((and (eq? (toBoolean? (mlist expr)) #f) (declared var state)) (M_stateAssign state var (M_value (mlist expr) state)))
      (else (error "variable not declared" var)))))


; checks if a variable has been declared
(define declared
  (λ (var state)
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
  (λ (name state)
    (cond
      ((and (null? (listVars (topLayer state))) (null? (removeLayer state))) (error "Variable not declared" name))
      ((null? (listVars (topLayer state))) (lookup name (removeLayer state)))
      ((and (equal? name (firstVar (topLayer state))) (eq? 'nul (firstVal (topLayer state)))) (error "Variable not assigned a value" name))
      ((equal? name (firstVar (topLayer state))) (firstVal (topLayer state)))
      (else (lookup name (cons (nextInState (topLayer state))(removeLayer state)))))))


;Adding a layer onto the state
(define newLayer
  (λ (state)
    (cons '(()()) state)))


;The top layer on a state
(define topLayer
  (λ(state)
    (car state)))


;The state without the top layer
(define removeLayer
  (λ(state)
    (cdr state)))


;All the variables in the top layer of the state
(define listVars
  (λ (state)
     (car state)))


;All the values of the variables in the top layer of the state
(define listVals
  (λ(state)
    (cadr state)))


;first variable in the state
(define firstVar
  (λ (state)
    (caar state)))


;value of the first variable
(define firstVal
  (λ (state)
    (caadr state)))


;"cdr" of the state
(define nextInState
  (λ (state)
    (cons (cdar  state) (cons (cdr(cadr state)) '()))))


;adding a variable that has been assigned a value to a state
(define addToState
  (λ (state a b)
    (cons (cons (cons a (listVars (topLayer state))) (cons (cons b (listVals(topLayer state))) '()))(removeLayer state))))


;adding a variable that has been declared but not assigned
(define declare
  (λ (state a)
    (cons (cons (cons a (listVars(topLayer state))) (cons (cons 'nul (listVals(topLayer state))) '()))(removeLayer state))))


;removing a variable from the state
(define removeState
  (λ (x state)
    (cond
      ((and (null? (listVars(topLayer state))) (null? (removeLayer state))) state)
      ((null? (listVars(topLayer state))) (cons (topLayer state)(removeState x (removeLayer state))))
      ((eq? x (firstVar (topLayer state))) (cons (nextInState(topLayer state))(removeLayer state)))
      (else (addToState (removeState x (cons (nextInState(topLayer state))(removeLayer state))) (firstVar(topLayer state)) (firstVal(topLayer state)))))))


;update a declared variable
(define updateVar
  (λ (x y state)
    (cond
      ((and (null? (listVars(topLayer state))) (null? (removeLayer state))) state)
      ((null? (listVars(topLayer state))) (cons (topLayer state)(updateVar x y (removeLayer state))))
      ((eq? x (firstVar (topLayer state))) (addToState (cons (nextInState (topLayer state)) (removeLayer state)) x y))
      (else (addToState (updateVar x y (cons (nextInState(topLayer state))(removeLayer state))) (firstVar(topLayer state)) (firstVal(topLayer state)))))))


; adds a variable to the state
(define M_stateAssign
  (λ (state a b)
    (if (declared a state)
        (updateVar a b state)
        (addToState (removeState a state) a b))))


; puts an atom in a list -> necessary to avoid errors
(define mlist
  (λ (x)
    (cons x '())))


; checks if a statement can be evaluated by M_boolean
(define toBoolean?
  (λ (lis)
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
;(define M_while
;   (λ (lis state break continue)
;     (cond
;       ((equal? 'break (car lis)) (break (M_while '() state)))
;       ((equal? 'continue (car lis)) (continue (M_while '() state)))
;       ((M_boolean (mlist (cadr lis)) state) (M_state lis (M_state (mlist (caddr lis)) state)))
;       (else (M_state '() state))
;       )))

; controls while statements
(define M_whileHelper
   (λ (myCond body state return break)
     (cond
       ((M_boolean myCond state) (M_whileHelper myCond body (M_state body state return break) return break))
       (else state)
       )))

(define M_while
  (λ (myCond body state return)
    (call/cc
     (λ(break)
       (M_whileHelper myCond body state return break)))))

;controls if statements
(define M_if
  (λ (lis state return break)
    (cond
      ((M_boolean (cadr lis) state) (M_state (caddr lis) state return break))
      ((eq? 3 (length lis)) state)
      ((eq? 4 (length lis)) (M_state (cadddr lis) state return break)))))
