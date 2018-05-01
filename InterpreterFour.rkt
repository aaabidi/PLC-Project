;#lang racket

(load "classParser.scm")
(require racket/trace)

; Setup:
; 1. Create initial-return, which accepts a statement and the environment from which the return
;    was called, evaluates the statement, and returns the result.
; 2. Create outer-environment, which contains all class definitions.
; 3. Create run, a function that calls the main method in class classname.
; Execution: Run begin-interpret. Pass it outer-environment with an empty layer for main's local
; variable and function definitions.

(define interpret
  (lambda (filename classname)
    (call/cc
      (lambda (return)
        (let* ((initial-return (lambda (statement env) (return (eval-expression (operand statement) env return default-break default-continue default-throw))))
               (outer-environment (interpret-classes (parser filename) initial-env (lambda (statement env) (return env)) default-break default-continue default-throw))
               (main-closure (lookup 'main (lookup (string->symbol classname) outer-environment)))
               (run (lambda (env) (do-interpret (getFuncBody main-closure) env initial-return default-break default-continue default-throw))))

              ; Begin interpreting. Pass in the environment, which is built by interpreting the outermost layer
              ; of the program, containing function and global variable definitions.
              (run (push-frame outer-environment)))))))

;(define main '((funcall main)))
;(define mainFuncall car)

; Recursively evaluate all class definitions in the statement list.
(define interpret-classes
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (interpret-classes (nextExpressions statement)
                         (interpret-class (firstExpression statement) state return break continue throw)
                         return break continue throw))))

; do-interpret takes statements, evaluates them, and changes the state recursively
(define do-interpret
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (do-interpret (nextExpressions statement)
                    (interpret-statement (firstExpression statement) state return break continue throw)
                    return break continue throw))))

(define initial-env '(((true false) (true false))))
(define default-break (lambda (s) (error 'badBreak "break can only be called in a loop")))
(define default-continue (lambda (s) (error 'badContinue "continue can only be called in a loop")))
(define default-throw (lambda (e s) (error 'uncaughtError "Error was  not caught")))

; interpret-class add a class from the given statement to the class environment
; the environment will have class and their defffenitions which include fields and functions/closures
(define interpret-class
  (lambda (statement class-state return break continue throw)
    (cond
      ((null? (has-super statement)) (insert (className statement) (append (do-interpret (body statement) initial-env return break continue throw) '(())) class-state))
      (else (insert (className statement) (append (do-interpret (body statement) initial-env return break continue throw) (cons (get-super statement) '())) class-state)))))

(define has-super caddr)
(define className cadr)
(define body cadddr)
(define get-super (lambda (v) (cadr (caddr v))))
(define innerParens car)

; Mstate modifies the state depending on the contents of statement, then returns the state..
; TODO: Move while's continuation to Mstate-while
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
      ((eq? (operator statement) 'static-function) (interpret-static-func statement state))
      ((eq? (operator statement) 'throw) (throw (eval-expression (exception statement) state return break continue throw)))
      ((eq? (operator statement) 'try) (interpret-tcf statement state return break continue throw))
      ((eq? (operator statement) 'var) (interpret-var statement state return break continue throw))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (interpret-while (whileCondition statement) (whileBody statement) state return new-break continue throw))))
      (else (error 'unknown "Encountered an unknown statement")))))

(define exception cadr)


(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; Handles assignment
(define interpret-assign
  (lambda (statement env r b c t)
    (cond
      ((list? (varOfstatement)) (interpret-assign-dot statement env r b c t))
      (else (replace_var (varOfstatement) (eval-expression (operation statement) env r b c t) env)))))

(define interpret-assign-dot
  (lambda (statement env r b c t)
    (cond
      ((eq? (assign-dot-env statement) 'this) (replace_var (assign-dot-var statement) (eval-expression (assignment-dot-value statement) env r b c t) (pop-frame env)))
      ((eq? (assign-dot-env statement) 'super) (replace_var (assign-dot-var statement) (eval-expression (assignment-dot-value statement) env r b c t) (pop-frame (pop-frame env))))
      (else (replace_var (assign-dot-var statement) (eval-expression (assign-dot-value statement) env r b c t) (lookup (assignment-dot-env statement)))))))

(define assign-dot-env cadadr)
(define assign-dot-var (lambda (v) (caddr (cadr v))))
(define assign-dot-value caddr)

;Handles Block Statements
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))
; handles if statements
; Statement format: (else-statement is optional)
; (if (condition) (statement) (else-statement))
(define interpret-if
  (lambda (statement env return break continue throw)
    (cond
      ((eq? 'true (Mbool (if-condition statement) env return break continue throw)) (interpret (if-statement statement) env return break continue throw))
      ((not (null? (else-statement-exists statement))) (interpret (else-statement statement) env return break continue throw))
      (else env))))

(define else-statement-exists cdddr)
(define if-condition cadr)
(define if-statement caddr)
(define else-statement cadddr)

; Mstate-func handles function declarations
; Statement format:
; (function function-name (formal-param-1, formal-param-2, ...) (body))
(define interpret-func
  (lambda (statement env)
    (cond
      ((exists? (getFuncName statement) env) (error 'redefining (format "function ~a has already been declared" (funcName statement))))
      (else (insert (getFuncName statement) (createClosure (getParams statement) (getBody statement)) env)))))


(define interpret-static-func
  (lambda (statement env)
    (cond
      ((exists? (getFuncName statement) env) (error 'redefining (format "function ~a has already been declared" (funcName statement))))
      (else (cons (car env) (insert (getFuncName statement) (createClosure (getParams statement) (getBody statement)) '((()()))))))))

;helper methods for Mstate-func
(define getFuncName cadr)
(define getParams caddr)

; When a function is called without the calling line needing its return
; value, execute the function and then return the environment.
; Statement format:
; (funcall function-name actual-param-1 actual-param-2 ...)
(define interpret-funcall
  (lambda (funcall env return break continue throw)
    (begin (Mvalue-funcall funcall env return break continue throw) env)))

;helpers for Mstate-funcall
(define globalStateOfEnvironment cdr)
(define getFuncBody cadr)
(define getFuncEnvironment caddr)
(define getBody cadddr)

; Modify the state based on a try-catch-finally block.
; Statement format, where each "body" can consist of multiple statements in a list:
; (try (try-body) (catch (exception-name) (catch-body)) (finally (finally-body)))
(define interpret-tcf
  (lambda (statement env return break continue throw)
    (call/cc
      (lambda (catch-continuation)
        (letrec ((finally (lambda (s)
                  (if (pair? (finally-stmt statement))
                      (interpret-begin (finally-body statement) s return break continue throw)
                      s)))
                (try (lambda (new-throw)
                  ; if this try block is accompanied by a catch block, pass a continuation that
                  ; jumps us to it when we encounter a throw. Otherwise, pass whatever throw continuation
                  ; we were passed when we entered this try block.
                  (if (pair? (catch-block statement))
                    (finally (interpret-begin (try-body statement) env return break continue new-throw))
                    (finally (interpret-begin (try-body statement) env return break continue throw)))))
                (catch (lambda (e s)
                  (finally (catch-begin (catch-body statement) (catch-err statement) e s return break continue throw)))))
                ; Call "try" with catch as the catch-continuation
                (try (lambda (e) (catch-continuation (catch e env)))))))))

; Same as Mstate-begin, but with the addition of inserting the exception into the
; environment before calling do-interpret.
(define catch-begin
  (lambda (statement e-name e-value env return break continue throw)
    (pop-frame (do-interpret statement
                                 (insert e-name e-value (push-frame env))
                                 return
                                 (lambda (s) (break (pop-frame s)))
                                 (lambda (s) (continue (pop-frame s)))
                                 throw))))

(define try-body cadr)
(define catch-body (lambda (v) (caddr (caddr v))))
(define catch-block caddr)
(define catch-err (lambda (v) (car (cadr (caddr v)))))
(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

; MState-var handles variable declaration
; Statement format:
; (var var-name) OR (var var-name value) OR (var var-name NEW Constructor)
(define interpret-var
  (lambda (statement env r b c t)
    (cond
      ((null? (thirdElement statement)) (insert (varOf statement) 'undefined env))
      ((not (list? (unNestIfValue (thirdElement statement)))) (insert (varOf statement) (eval-expression (operation statement) env r b c t) env))
      (else (insert (varOf statement) (get-class-env (class-type statement) env) env)))))

(define class-type (lambda (v) (car (cdaddr v))))
(define unNestIfValue car)

;get-class-env will take a class and and environment and return the closure for the object of that class
;class will be the name of the class and env will be the environment
(define get-class-env
  (lambda (class-name env)
    (cond
      ((no-parent-no-static (lookup class-name env)) (cons (class-env (lookup class-name env)) env))
      ((has-parent-no-static (lookup class-name env)) (cons (class-env (lookup class-name env)) (get-class-env (get-parent-no-static (lookup class-name env)) env)))
      ((no-parent-has-static (lookup class-name env)) (cons (class-env (lookup class-name env)) env))
      (else (cons (class-env (lookup class-name env)) (get-class-env (get-parent-has-static (lookup class-name env)) env))))))

(define no-parent-no-static (lambda (v) (null? (cadr v))))
(define has-parent-no-static (lambda (v) (not (list? (cadr v)))))
(define no-parent-has-static (lambda (v) (null? (caddr v))))
(define class-env car)
(define get-parent-no-static cadr)
(define get-parent-has-static caddr)

; Mstate-while handles while loops
; TODO: check that continue actually works
; Statement format:
; (while (condition) (body))
; body may be one line only; for multiple lines, it must contain a begin.
(define interpret-while
  (lambda (condition statement env return break continue throw)
    (if (eq? 'true (Mbool condition env return break continue throw))
      (interpret-while condition
                    statement
                    (call/cc
                      (lambda (new-continue)
                        (interpret statement env return break new-continue throw)))
                    return
                    break
                    continue
                    throw)
      env)))

(define whileCondition cadr)
(define whileBody caddr)

; Mvalue: Evaluate an expression to determine its value.
; Last params are return break continue throw. Shortened for brevity.
(define eval-expression
  (lambda (expr env r b c t)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) 'true)
      ((eq? expr 'false) 'false)
      ((not (list? expr)) (lookup expr env))
      ((eq? (operator expr) 'funcall) (evaluate-funcall expr env r b c t))
      ((eq? (operator expr) 'dot) (evaluate-dot expr env r b c t))
      (else (eval-operator expr environment return break continue throw)))))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand operand1) ; TODO: Can this be moved / replaced?

;Mvalue-dot gets the value based of of the specified environment
;it has a different case for handleing the possible operands before the dot
(define evaluate-dot
  (lambda (statement env r b c t)
    (cond
      ((eq? (operand1 statement) 'this) (eval-expression (operand2 statement) (pop-frame env) r b c t))
      ((eq? (operand1 statement) 'super) (eval-expression (operand2 statement) (pop-frame (pop-frame env)) r b c t))
      (else (eval-expression (operand2 statement) (lookup (operand1 statement) env) r b c t)))))

;Mvalue-funcall gets the value of a function call
;this function takes the statemetn and creats a usable statement for the old Mvalue-funcall by parsing and reorderin g and then getting the correct environment
(define evaluate-funcall
  (lambda (statement env return break continue throw)
    (cond
      ((eq? (class-type-of-function statement) 'this) (eval-funcall-with-env (append (cons 'funcall (cons (function-call statement) '())) (params-of-funcall statement)) env return break continue throw))
      ((eq? (class-type-of-function statement) 'super) (eval-funcall-with-env (append (cons 'funcall (cons (function-call statement) '())) (params-of-funcall statement)) (pop-frame env) return break continue throw))
      (else (eval-funcall-with-env (append (cons 'funcall (cons (function-call statement) '())) (params-of-funcall statement)) (lookup (class-type-of-function statement) env) return break continue throw)))))

(define function-call (lambda (v) (car (cddadr v))))
(define params-of-funcall cddr)
(define class-type-of-function cadadr)
; When a function is called, Mvalue-funcall does the following:
; 1. Creates the function's execution environment using the environment function stored
;    in the function closure
; 2. Binds the actual parameters to the formal parameters in the new environment
; 3. Evaluates the body of the function.
;
; Differing
; Execute a function and return the value produced by its return statement.
; TODO: Match env-contains-symbol? check from Mstate-funcall
(define eval-funcall-with-env
  (lambda (expr env return break continue throw)
    (call/cc
      (lambda (new-return)
        (let* ((func-name (getFuncName expr))
              (function (lookup func-name env)))
              (do-interpret (getFuncBody function)
                            ; replace the below with a call to the function closure's create-env function
                            ; function in the closure should already pass the function name into getFunctionExecutionEnvironment
                            ; so that we don't have to do it here
                            (functionExecutionEnvironment expr env return break continue throw)
                            (lambda (expr env) (new-return (eval-expression (operand expr) env return break continue throw)))
                            break
                            continue
                            throw))))))



; getFunctionExecutionEnviroinment gets the execution environment for a function call,
; which includes everything available to the function through static scoping along with
; its parameters.
; Assumes funcall is of format (funcall methodName actual-param-1 actual-param-2 ...)
; Return looks like
; (((formal-param-names)(actual-param-values)) ((declaration-scope-symbols)(declaration-scope-values)) ... ((global-symbols)(global-values)))
(define functionExecutionEnvironment
  (lambda (funcall env r b c t)
    (cons (bindParameters (function-name funcall) (paramsOf funcall) env r b c t) (functionDeclarationEnvironment (function-name funcall) env))))

(define function-name cadr)
(define paramsOf cddr)

; Returns an environment with all bindings within the function's scope - i.e.,
; all bindings available in the layer it was declared and above. Does not prepend
; an empty local scope. Based on static scoping.
; Return looks like:
; (((declaration-scope-symbols)(declaration-scope-values)) ... ((global-symbols)(global-values)))
(define functionDeclarationEnvironment
  (lambda (funName env)
    (if(env-contains-symbol? funName (top-frame-variables env))
      env
      (functionDeclarationEnvironment funName (remainingframes env)))))

; Given the name of a function, the actual parameters being passed to the function,
; and the environment from which the function was called, locate the function closure
; in env and bind the actual parameters to the function's formal parameters.
; Return looks like:
; ((formal-param-names)(actual-param-values))
(define bindParameters
  (lambda (funcName actualParams env r b c t)
    (bindActualToFormal (getParamsFromEnvironment funcName env) actualParams env '(()()) r b c t)))

; Returns the list of formal parameters as stored in the function closure in the environment.
(define getParamsFromEnvironment
  (lambda (funName env)
    (car (lookup funName env))))

; Recursively bind the actual parameters to the formal parameters.
; Accepts the environment from which the function is being called and localEnv, which should
; be '(()()) on the first call.
; Not a great name but... meh
(define bindActualToFormal
  (lambda (formalParams actualParams env localEnv r b c t)
    (cond
      ; If we've reached the end of the formal or actual param list but not the other, the
      ; function was not called with the correct number of parameters and we throw an error.
      ((and (null? formalParams) (not (null? actualParams))) (error 'methodSignature (format "too many parameters")))
      ((and (not (null? formalParams)) (null? actualParams)) (error 'methodSignature (format "too few parameters")))
      ((null? formalParams) localEnv)
      (else (bindActualToFormal (nextParams formalParams)
                                (nextParamValues actualParams)
                                env
                                (topframe (insert (currentParam formalParams) (eval-expression (currentParamValue actualParams) env r b c t) (cons localEnv '())))
                                r b c t)))))

;helpers for bindActualToFormal
(define nextParams cdr)
(define nextParamValues cdr)
(define currentParam car)
(define currentParamValue car)

; Mbool: Evaluate a statement for a truth value of true or false.
(define Mbool
  (lambda (statement state r b c t)
    (cond
      ((not (list? statement)) (eval-expression statement state r b c t))
      ((eq? statement 'true) 'true)
      ((eq? statement 'false) 'false)
      ((not (list? statement)) (eval-expression statement state r b c t))
      ((eq? (comparator statement) '>) (if (> (eval-expression (operand1 statement) state r b c t) (eval-expression (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '<) (if (< (eval-expression (operand1 statement) state r b c t) (eval-expression (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '>=) (if (>= (eval-expression (operand1 statement) state r b c t) (eval-expression (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '<=) (if (<= (eval-expression (operand1 statement) state r b c t) (eval-expression (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '==) (if (= (eval-expression (operand1 statement) state r b c t) (eval-expression (operand2 statement) state r b c t)) 'true 'false))
      ((eq? (comparator statement) '!=) (if (not (= (eval-expression (operand1 statement) state r b c t) (eval-expression (operand2 statement) state r b c t))) 'true 'false))
      ((eq? (comparator statement) 'funcall) (eval-expression statement state r b c t))
      ((eq? (operator statement) '&&) (if (eq? #t (and (eq? 'true (Mbool (operand1 statement) state r b c t)) (eq? 'true (Mbool (operand2 statement) state r b c t)))) 'true 'false))
      ((eq? (operator statement) '||) (if (eq? #t (or (eq? 'true (Mbool (operand1 statement) state r b c t)) (eq? 'true (Mbool (operand2 statement) state r b c t)))) 'true 'false))
      ((eq? (operator statement) '!) (if (eq? #t (not (eq? 'true (Mbool (operand1 statement) state r b c t)))) 'true 'false))
      (else (error 'invalidInput "This expression cannot be evaluated to a boolean value")))))

(define comparator car)


; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
;THIS MIGHT BE WRONG

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment return break continue throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment return break continue throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment return break continue throw) environment return break continue throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment return break continue throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment return break continue throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment return break continue throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

; HELPER METHODS

;helpers for lookup
(define remainingframes cdr)

(define variableList caar)

; remove removes a variable from the state
; it takes the variable name and the state and removes it from the state
(define replace_var
  (lambda (var value state)
    (cond
      ((null? state) (error 'out-of-scope (format "Symbol ~a is out of scope or does not exist" var)))
      ((env-contains-symbol? var (top-frame-variables state)) (cons (get_replaced var value (currentLayer state)) (remainingframes state)))
      (else (cons (topframe state) (replace_var var value (remainingframes state)))))))

(define get_replaced
  (lambda (var value state)
    (cond
      ((eq? (variable1 state) var) (cons (cons var (nextVars state)) (cons (cons (begin (set-box! (valueOfVar1 state) value) (valueOfVar1 state)) (nextValues state)) '())))
      (else (topframe (insert (variable1 state) (unbox (valueOfVar1 state)) (cons (get_replaced var value (cons (nextVars state) (cons (nextValues state) '()))) '())))))))

;insert inerts a variable into the state, if the value already exists it replaces it
;returns the state with a given variable and value added in
;(define insert
;  (lambda (var value state)
;    (cons (cons (cons var (variables state)) (cons (cons (box value) (valuesInState state)) '())) (cdr state))))

;createClosure creates a closure functon that will be added to the state
;the thirsd part of the cosure is the framework for the environment
(define createClosure
  (lambda (params body)
    (cons params (cons body (cons functionExecutionEnvironment '())))))

;exists?? checks if the variable has already been declared in the state
(define exists?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((env-contains-symbol? var (top-frame-variables state)) #t)
      (else (exists? var (remainingframes state))))))

(define env-contains-symbol?
  (lambda (var varList)
    (cond
     ((null? varList) #f)
     ((eq? var (firstVar varList)) #t)
     (else (env-contains-symbol? var (cdr varList))))))

;helper for state contains
(define firstVar car)

(define resOfVariablesInState cdr)

;adds a level of scope to the given state
(define push-frame
  (lambda (state)
    (cons '(()()) state)))

;gets the first variable in the state
(define variable1 caar)

;gets the value associated with the first variable in the state
(define valueOfVar1 caadr)

;rest of the variables in the state
(define nextVars cdar)

;rest of the values in the state
(define nextValues cdadr)

;get the values in the state
(define allValues cadar)

;the expression in the stat of the program
(define firstExpression car)

;the rest of the expressions in the programs
(define nextExpressions cdr)

;variable
(define varOf cadr)

;third element
(define thirdElement cddr)

;operation
(define operation caddr)



;-----------------
; HELPER FUNCTIONS
;-----------------

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
  (lambda (environment)
    (cons newframe environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))

; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

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
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

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

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

(define top-frame-variables
  (lambda (environment)
    (variables (topframe environment))))

(define top-frame-store
  (lambda (environment)
    (cadr (top-frame environment))))


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
