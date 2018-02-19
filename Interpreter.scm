(define M_program
  (lambda (lis)
    (cond
      ((not (null? lis)) ((M_state (car lis)) (M_program (cdr lis)))))))


;"cdr" of the state
(define nextInState
  (lambda (state)
    (cons (cdar state) (cons (cdr(cadr state)) '()))))

;consing onto a state
(define addToState
  (lambda (state a b)
    (cons (cons a (car state)) (cons (cons b (cadr state)) '()))))


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

; Ammar - I commented this out because I didn't see it and wrote my own with error handling (M_valVar)
;getValue returns the value of a variable from a state
;(define getValue
; (lambda (var state)
;    (cond
;      ((eq? var (caar state)) (car (cadr state)))
;      (else (getValue var (cons (cdar state) (cons (cdr (car (cdr state))) '())))))))


;M_value According to the assignment the parser gives operations in prefix notation
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
    ((eq? '/ (car lis)) (/ (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
    ((eq? '% (car lis)) (modulo (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
    ;Check for a letter and then go to lookup, but the else case should go all the way back to M_state
    (else (lookup (car lis) state)))))
;(M_state '(+ (* 3 x) 4) '((x) (5)))



;(M_boolean '(> (* x (+ x x)) y) ((x y) (3 50)))
;(M_boolean '(== true (>= (+ 3 7) (+ 4 10))) '(()()))
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
      ((eq? '!  (car lis)) (not (M_state (mlist(cadr lis)) state) (M_state (mlist(caddr lis)) state)))
      ((eq? 'true (car lis)) #t)
      ((eq? 'false (car lis)) #f)
      (else (M_state lis state)))))

(define M_state
  (lambda (lis state)
    (cond
      ((null? lis) (error "input not a statement" lis))
      ((list? (car lis)) (M_state (car lis) state))
      ;Declaration mboolean needs to be changed to mstate?
      ((and (equal? (length lis) 3) (eq? 'var    (car lis))) (M_stateAssign state (cadr lis) (M_boolean (mlist (caddr lis)) state)))
      ;Null may need to change later
      ((and (equal? (length lis) 2) (eq? 'var    (car lis))) (M_stateAssign state (cadr lis) null))
      ;Works without assignment so it needs to be tweaked
      ((eq? '=      (car lis)) (M_value (mlist (cadr lis)) (M_stateAssign state (cadr lis) (M_state (mlist (caddr lis)) state))))
      ;return
      ((eq? 'return (car lis)) (M_state (cadr lis) state))
      ;if
      ((eq? 'if (car lis)) (M_state-if lis state))
      ;while
      ((eq? 'while (car lis)) (M_state-while lis state))
      ;toBoolean
      ((toBoolean? lis) (M_boolean lis state))
      ;toValue
      ((toValue? lis) (M_value lis state))
      )))




;Getting the decimal value of a number represented with each digit as an element in a list
(define M_valNum
  (lambda (num)
    (cond
      ;returning 0 might be a bad idea
      ((null? num) '0)
      (else (+ (MValNum (cdr num)) (* (car num) (expt 10 (- (length num) 1))))))))
;Tests for M_valNum
;(M_valNum '())
;(M_valNum '(3 5 9))
;(M_valNum '(0 1 0 0 1 0))


;finds the value of a variable given a state
(define M_valVar
  (lambda (name state)
    (cond
      ((null? (car state)) (error "Variable with specified name not found" name) )
      ((equal? name (caar state)) (caar (cdr state)))
      (else ( M_valVar name (nextInState state))))))
;Tests for M_valVar
;(M_valVar 'c '((a b c)(1 2 3)))
;(M_valVar 'z '((a b c)(1 2 3)))


;finds the value of a variable given a state
(define lookup
  (lambda (name state)
    (cond
      ((null? (car state)) (error "Variable with specified name not found" name) )
      ((equal? name (caar state)) (caar (cdr state)))
      (else (lookup name (nextInState state))))))


(define mlist
  (lambda (x)
    (cons x '())))


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


(define toValue?
  (lambda (lis)
    (cond
      ((null? lis) (error "input is not a valid speaker"))
      ((eq? '+ (car lis)) #t)
      ((eq? '- (car lis)) #t)
      ((eq? '* (car lis)) #t)
      ((eq? '/ (car lis)) #t)
      ((eq? '% (car lis)) #t)
      ((eq? '+ (car lis)) #t)
      ((eq? '+ (car lis)) #t)
      ((eq? '+ (car lis)) #t)
      (else #t))))


;Conditional statement
;Takes a list which is the entire if-else and the state
(define MState-if
  (lambda (lis state)
    (if (M_boolean (if-condition lis) state)
       (M_state (then lis) state)
       (M_state (else* lis) state))))
    

;functions for determining which elements of a list (if statement) are the conditional and the lines to execute
(define if-condition
  (lambda (lis)
    ((cadr lis))))

(define then
  (lambda(lis)
    ((caddr lis))))

(define else*
  (lambda (lis)
    ((cadddr lis))))

;while loop
;(caddr lis) is the loop body
;(cadr lis) is the loop condition
;executing the loop body changes the state. 

(define M_state-while
  (lambda (lis state)
    (cond
      ((M_boolean (cadr lis) state) (M_state-while lis (M_state (caddr lis) state)))
      (else (cdddr lis)))))

