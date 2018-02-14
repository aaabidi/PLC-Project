;"cdr" of the state
(define nextInState
  (lambda (state)
    (cons (cdar state) (cons (cdr(cadr state)) '()))))

;consing onto a state
(define addToState
  (lambda (state a b)
    (cons (cons a (car state)) (cons (cons b (cadr state)) '()))))

;finds the value of a variable given a state
(define lookup
  (lambda (name state)
    (cond
      ((null? (car state)) (error "Variable with specified name not found" name) )
      ((equal? name (caar state)) (caar (cdr state)))
      (else (lookup name (nextInState state))))))

;removing a variable from the state
(define removeState
  (lambda (x state)
    (cond
      ((null? (car state)) state)
      ((eq? x (caar state)) (nextInState state))
      (else (addToState (removeState x (nextInState state)) (caar state) (caadr state))))))

; adds a variable to the state
(define MStateAssign
  (lambda (state a b)
    (MStateAssign (removeState a state) a b)))

;getValue returns the value of a variable from a state
(define getValue
  (lambda (var state)
    (cond
      ((eq? var (caar state)) (car (cadr state)))
      (else (getValue var (cons (cdar state) (cons (cdr (car (cdr state))) '())))))))

;M_value According to the assignment the parser gives operations in prefix notation
(define M_value
  (lambda (lis state)
    (cond
      ((list? (car lis)) (M_value (car lis)))
      ((digit? (car lis)) (M_digit (car lis)))
      ((eq? '+ (car lis)) (+ (M_value (cadr lis)) (M_value (caddr lis))))
      ((eq? '- (car lis)) (- (M_value (cadr lis)) (M_value (caddr lis))))
      ((eq? '* (car lis)) (* (M_value (cadr lis)) (M_value (caddr lis))))
      ((eq? '/ (car lis)) (/ (M_value (cadr lis)) (M_value (caddr lis))))
      ((eq? '% (car lis)) (modulo (M_value (cadr lis)) (M_value (caddr lis))))
      (else (getValue (car lis) state)))))
      
;Getting the decimal value of a number represented with each digit as an element in a list
(define MValNum
  (lambda (num)
    (cond
      ;returning 0 might be a bad idea
      ((null? num) '0)
      (else (+ (MValNum (cdr num)) (* (car num) (expt 10 (- (length num) 1))) )) )))    
;Tests for MValNum
;(MValNum '())
;(MValNum '(3 5 9))
;(MValNum '(0 1 0 0 1 0))

