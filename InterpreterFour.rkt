;#lang racket
(load "classParser.scm")
(require racket/trace)

(define interpret
  (lambda (filename classname)
    (call/cc
      (lambda (return)
        (let* ((initial-return (lambda (statement env) (return (eval-expression (operand statement) env return default-break default-continue default-throw))))
               (outer-environment (interpret-classes (parser filename) newenvironment (lambda (statement env) (return env)) default-break default-continue default-throw))
               (main-closure (lookup 'main (lookup (string->symbol classname) outer-environment)))
               (run (lambda (env) (interpret-do (getFuncBody main-closure) env initial-return default-break default-continue default-throw))))
               (run (push-frame outer-environment)))))))

; interprets code in classes
(define interpret-classes
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (interpret-classes (nextExpressions statement) (interpret-class (firstExpression statement) state return break continue throw) return break continue throw))))

; updates state with evaluated statements
(define interpret-do
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (interpret-do (nextExpressions statement)
                    (interpret-statement (firstExpression statement) state return break continue throw)
                    return break continue throw))))

; interpret-class add a class from the given statement to the class environment
(define interpret-class
  (lambda (statement class-state return break continue throw)
    (cond
      ((null? (hasParent statement)) (insert (className statement) (append (interpret-do (body statement) newenvironment return break continue throw) '(())) class-state))
      (else (insert (className statement) (append (interpret-do (body statement) newenvironment return break continue throw) (cons (getParent statement) '())) class-state)))))

(define hasParent caddr)
(define className cadr)
(define body cadddr)
(define getParent (lambda (v) (cadr (caddr v))))
(define innerParens car)

; Deals with the state of individual statements (interprets an individual statement)
(define interpret-statement
  (lambda (statement state return break continue throw)
    (cond
      ((eq? (operator statement) '=) (interpret-assign statement state return break continue throw))
      ((eq? (operator statement) 'begin) (interpret-block statement state return break continue throw))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'funcall) (interpret-funcall statement state return break continue throw))
      ((eq? (operator statement) 'function) (interpret-func statement state))
      ((eq? (operator statement) 'if) (interpret-if statement state return break continue throw))
      ((eq? (operator statement) 'return) (return statement state))
      ((eq? (operator statement) 'throw) (throw (eval-expression (exception statement) state return break continue throw)))
      ((eq? (operator statement) 'try) (interpret-tcf statement state return break continue throw))
      ((eq? (operator statement) 'var) (interpret-var statement state return break continue throw))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (interpret-while (whileCondition statement) (whileBody statement) state return new-break continue throw))))
      (else (error 'badOperator "Cannot evaluate this statement")))))

(define exception cadr)

; Uses interpret statement to interpret a whole statement list
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; Handles assignment statements
(define interpret-assign
  (lambda (statement env return break continue throw)
    (cond
      ((list? (varOfstatement)) (interpret-assign-dot statement env return break continue throw))
      (else (replace_var (varOfstatement) (eval-expression (operation statement) env return break continue throw) env)))))

; Handles dot assignment statements
(define interpret-assign-dot
  (lambda (statement env return break continue throw)
    (cond
      ((eq? (dot-env statement) 'this) (replace_var (dot-var statement) (eval-expression (dot-val statement) env return break continue throw) (pop-frame env)))
      ((eq? (dot-env statement) 'super) (replace_var (dot-var statement) (eval-expression (dot-val statement) env return break continue throw) (pop-frame (pop-frame env))))
      (else (replace_var (dot-var statement) (eval-expression (dot-val statement) env return break continue throw) (lookup (assignment-dot-env statement)))))))

; Interprets block statements
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement) (push-frame environment) return (lambda (env) (break (pop-frame env))) (lambda (env) (continue (pop-frame env))) (lambda (v env) (throw v (pop-frame env)))))))

; Interprets if statements
(define interpret-if
  (lambda (statement env return break continue throw)
    (cond
      ((eq? 'true (eval-binary-op2 (if-condition statement) env return break continue throw)) (interpret (if-statement statement) env return break continue throw))
      ((not (null? (else-statement-exists statement))) (interpret (else-statement statement) env return break continue throw))
      (else env))))

; Interprets functions
(define interpret-func
  (lambda (statement env)
    (cond
      ((exists? (getFuncName statement) env) (error 'nameTaken "a function name was used twice"))
      (else (insert (getFuncName statement) (createClosure (getParams statement) (getBody statement)) env)))))

; Interprets function calls
(define interpret-funcall
  (lambda (funcall env return break continue throw)
    (begin (Mvalue-funcall funcall env return break continue throw) env)))

; Interprets variable declarations
(define interpret-var
  (lambda (statement env return break continue throw)
    (cond
      ((null? (thirdElement statement)) (insert (varOf statement) 'undefined env))
      ((not (list? (car (thirdElement statement)))) (insert (varOf statement) (eval-expression (operation statement) env return break continue throw) env))
      (else (insert (varOf statement) (get-class-env (class-type statement) env) env)))))

; Interprets while statements
(define interpret-while
  (lambda (condition statement env return break continue throw)
    (if (eq? 'true (eval-binary-op2 condition env return break continue throw))
      (interpret-while condition
        statement
        (call/cc
          (lambda (new-continue)
            (interpret statement env return break new-continue throw))) return break continue throw) env)))

(define class-type (lambda (v) (car (cdaddr v))))

;take a class name and environment and return the closure for that class
(define get-class-env
  (lambda (classname env)
    (cond
      ((no-parent (lookup classname env)) (cons (class-env (lookup classname env)) env))
      (else (cons (class-env (lookup classname env)) (get-class-env (get-parent (lookup classname env)) env))))))

; Evaluates individual expressions
(define eval-expression
  (lambda (expr env return break continue throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) 'true)
      ((eq? expr 'false) 'false)
      ((not (list? expr)) (lookup expr env))
      ((eq? (operator expr) 'funcall) (evaluate-funcall expr env return break continue throw))
      ((eq? (operator expr) 'dot) (evaluate-dot expr env return break continue throw))
      (else (eval-operator expr env return break continue throw)))))

; Evaluates the value of a dot expression in a given environment
(define evaluate-dot
  (lambda (statement env return break continue throw)
    (cond
      ((eq? (operand1 statement) 'this) (eval-expression (operand2 statement) (pop-frame env) return break continue throw))
      ((eq? (operand1 statement) 'super) (eval-expression (operand2 statement) (pop-frame (pop-frame env)) return break continue throw))
      (else (eval-expression (operand2 statement) (lookup (operand1 statement) env) return break continue throw)))))

; Evaluates the value of a function call
(define evaluate-funcall
  (lambda (statement env return break continue throw)
    (cond
      ((eq? (class-type-of-function statement) 'this) (eval-funcall-with-env (append (cons 'funcall (cons (function-call statement) '())) (params-of-funcall statement)) env return break continue throw))
      ((eq? (class-type-of-function statement) 'super) (eval-funcall-with-env (append (cons 'funcall (cons (function-call statement) '())) (params-of-funcall statement)) (pop-frame env) return break continue throw))
      (else (eval-funcall-with-env (append (cons 'funcall (cons (function-call statement) '())) (params-of-funcall statement)) (lookup (class-type-of-function statement) env) return break continue throw)))))

; Evaluates a function call with a given environment
(define eval-funcall-with-env
  (lambda (expr env return break continue throw)
    (call/cc
      (lambda (new-return)
        (let* ((func-name (getFuncName expr))
              (function (lookup func-name env)))
              (interpret-do (getFuncBody function)
                            (functionEnv expr env return break continue throw)
                            (lambda (expr env) (new-return (eval-expression (operand expr) env return break continue throw)))
                            break continue throw))))))

; REturns the environment for a function call
(define functionEnv
  (lambda (funcall env return break continue throw)
    (cons (bindParameters (function-name funcall) (paramsOf funcall) env return break continue throw) (functionDeclarationEnvironment (function-name funcall) env))))

; Abstractions for the function call environment function
(define function-name cadr)
(define paramsOf cddr)

; Returns the environment of a function declaration
(define functionDeclarationEnvironment
  (lambda (funName env)
    (if(in-env? funName (top-frame-variables env))
      env
      (functionDeclarationEnvironment funName (remainingframes env)))))

; Binds actual parameter values to parameter names
(define bindParameters
  (lambda (funcName actualParams env return break continue throw)
    (bindActualToFormal (getParamsFromEnvironment funcName env) actualParams env '(()()) return break continue throw)))

; Returns the list of formal parameters from input environment
(define getParamsFromEnvironment
  (lambda (funName env)
    (car (lookup funName env))))

; Helper method for bindParameters
(define bindActualToFormal
  (lambda (formalParams actualParams env localEnv return break continue throw)
    (cond
      ((and (null? formalParams) (not (null? actualParams))) (error 'badParams (format "parameter mistmatch")))
      ((and (not (null? formalParams)) (null? actualParams)) (error 'badParams (format "parameter mismatch")))
      ((null? formalParams) localEnv)
      (else (bindActualToFormal (nextParams formalParams (nextParamValues actualParams) env (topframe (insert (currentParam formalParams) (eval-expression (currentParamValue actualParams) env  return break continue throw) (cons localEnv '()))) return break continue throw))))))

(define eval-operator
  (lambda (expr environment return break continue throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) env return break continue throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) env)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) env return break continue throw) env return break continue throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value env return break continue throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) env return break continue throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) env return break continue throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) env return break continue throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

; remove removes a variable from the state
; it takes the variable name and the state and removes it from the state
(define replace_var
  (lambda (var value state)
    (cond
      ((null? state) (error 'badVariable "A variable was not found"))
      ((in-env? var (top-frame-variables state)) (cons (get_replaced var value (currentLayer state)) (remainingframes state)))
      (else (cons (topframe state) (replace_var var value (remainingframes state)))))))

(define get_replaced
  (lambda (var value state)
    (cond
      ((eq? (variable1 state) var) (cons (cons var (nextVars state)) (cons (cons (begin (set-box! (valueOfVar1 state) value) (valueOfVar1 state)) (nextValues state)) '())))
      (else (topframe (insert (variable1 state) (unbox (valueOfVar1 state)) (cons (get_replaced var value (cons (nextVars state) (cons (nextValues state) '()))) '())))))))

;createClosure creates a closure functon that will be added to the state
(define createClosure
  (lambda (params body)
    (cons params (cons body (cons functionEnv '())))))

;exists?? checks if the variable has already been declared in the state
(define exists?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((exists-in-list? var (top-frame-variables state)) #t)
      (else (exists? var (remainingframes state))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))


; add a frame onto the top of the environment
(define push-frame
  (lambda (state)
    (cons '(()()) state)))

;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe  '(() ()))

; add a frame onto the top of the environment
(define push-frame
  (lambda (env)
    (cons newframe env)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr env)))

; does a variable exist in the environment?
(define exists?
  (lambda (var env)
    (cond
      ((null? env) #f)
      ((exists-in-list? var (variables (topframe env))) #t)
      (else (exists? var (remainingframes env))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var env)
    (lookup-variable var env)))

; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var env)
    (let ((value (lookup-in-env var env)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var env)
    (cond
      ((null? env) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe env))) (lookup-in-frame var (topframe env)))
      (else (lookup-in-env var (cdr env))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l)))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val env)
    (if (exists-in-list? var (variables (car env)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car env)) (cdr env)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val env)
    (if (exists? var env)
        (update-existing var val env)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val env)
    (if (exists-in-list? var (variables (car env)))
        (cons (update-in-frame var val (topframe env)) (remainingframes env))
        (cons (topframe env) (update-existing var val (remainingframes env))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist))(begin (set-box! (car vallist) (scheme->language val)) (cons (car vallist) (cdr vallist))))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; gets variables from frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

(define top-frame-variables
  (lambda (env)
    (variables (topframe env))))

; alternates between #t/#f and true and false
(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

(define default-break (lambda (s) (error 'badBreak "break can only be called in a loop")))
(define default-continue (lambda (s) (error 'badContinue "continue can only be called in a loop")))
(define default-throw (lambda (e s) (error 'uncaughtError "error was  not caught")))

;-----------------
; HELPER FUNCTIONS
;-----------------

; Some general abstractions
(define variable1 caar)
(define valueOfVar1 caadr)
(define nextVars cdar)
(define nextValues cdadr)
(define allValues cadar)
(define firstExpression car)
(define nextExpressions cdr)
(define varOf cadr)
(define thirdElement cddr)
(define operation caddr)

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

;abstractions for lookup
(define remainingframes cdr)
(define variableList caar)

;  abstractions for frames
(define topframe car)
(define remainingframes cdr)

; abstractions for bindActualToFormal
(define nextParams cdr)
(define nextParamValues cdr)
(define currentParam car)
(define currentParamValue car)

; abstractions for eval expressions(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand operand1)

;helper for in-env?
(define firstVar car)
(define resOfVariablesInState cdr)

; Abstractions for the function call environment function
(define function-name cadr)
(define paramsOf cddr)

; abstractions for function calls
(define function-call (lambda (v) (car (cddadr v))))
(define params-of-funcall cddr)
(define class-type-of-function cadadr)

;abstractions for class evaluation
(define no-parent (lambda (v) (null? (cadr v))))
(define has-parent (lambda (v) (not (list? (cadr v)))))
(define class-env car)
(define get-parent cadr)
(define whileCondition cadr)
(define whileBody caddr)

;helpers for interpret-funcall
(define globalStateOfEnvironment cdr)
(define getFuncBody cadr)
(define getFuncEnvironment caddr)
(define getBody cadddr)

;abstractions methods for evaluation
(define getFuncName cadr)
(define getParams caddr)

; abstractions for if statements
(define else-statement-exists cdddr)
(define if-condition cadr)
(define if-statement caddr)
(define else-statement cadddr)
