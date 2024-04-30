module Compiler

// Find the position of variable x on the runtime stack
// If the variable is not found, it throws an error indicating that the variable is unbound.
let rec varpos x = function
    | []       -> failwith ("unbound: " + x)
    | y :: env -> if x = y then 0 else 1 + varpos x env // Check current or recurse deeper

// Adds new variables to the stack (aka: extends the stack)
let extend env x = x :: env

// Generating fresh labels:
// ensures unique labels throughout the code
let mutable labelCounter = 0
let newLabel() =
    let this = labelCounter
    labelCounter <- this + 1;
    "L" + string(this)

// Compile an expression recursively according to its type defined in the syntax
// Each expression type corresponds to one or more assembly instructions
let rec comp env = function
    | Syntax.INT i           -> [Asm.IPUSH i]                               // Push integer literal onto the stack
    | Syntax.VAR x           -> [Asm.ILOAD (varpos x env)]                  // Load variable from stack position
    | Syntax.BOOL b          -> if b then [Asm.IPUSH 1] else [Asm.IPUSH 0]  // Push boolean as integer

    // Arithmetic operations
    | Syntax.ADD (e1, e2)    -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IADD]
    | Syntax.MUL (e1, e2)    -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IMUL]
    | Syntax.SUB (e1, e2)    -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ISUB]
    | Syntax.DIV (e1, e2)   ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IDIV]
    | Syntax.MOD (e1, e2)   ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IMOD]

    // Logical operations including handling of non-equality and greater/less comparisons
    | Syntax.EQ (e1, e2)    ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ]
    | Syntax.NEQ (e1, e2)   ->  // pushes Asm.IEQ onto the stack, then pushed 0 onto the stack, then compares the values. If e1 != e2, then the final Asm.IEQ will push 1
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ] @
                                [Asm.IPUSH 0] @
                                [Asm.IEQ]
    | Syntax.LT (e1, e2)    ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ILT]
    | Syntax.LTE (e1, e2)   ->   // if less than is true, push 1; if it's false, check equality - if it's equal push 1, else push 0
                                let labelTrue = newLabel()
                                let labelFalse = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ILT] @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IJMP labelFalse] @
                                // if e1 == e2 is true, evaluate e1<=e2 to true, else to false
                                [Asm.ILAB labelFalse] @ 
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ] @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IPUSH 0] @
                                [Asm.IJMP labelEnd]@
                                // if e1 < e2 is true, evaluate e1<=e2 to true
                                [Asm.ILAB labelTrue] @ 
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd]@
                                [Asm.ILAB labelEnd]
    | Syntax.GT (e1, e2)    ->  // Pushes zero if e1 < e2 or e1 = e2, else pushes 1
                                let labelLessOrEqual= newLabel()
                                let labelNotLessThan = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @ // if e1 < e2 push 0
                                comp ("" :: env) e2 @
                                [Asm.ILT] @ 
                                [Asm.IJMPIF labelLessOrEqual] @
                                [Asm.IJMP labelNotLessThan] @

                                [Asm.ILAB labelNotLessThan] @ 
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ] @
                                [Asm.IJMPIF labelLessOrEqual] @
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelLessOrEqual] @ // if e1 < e2 is true, evaluate e1>e2 to false
                                [Asm.IPUSH 0] @
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelEnd]
    | Syntax.GTE (e1, e2)   ->  // Negation of less than
                                let labelLessThan= newLabel()
                                let labelEnd= newLabel()
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ILT] @
                                [Asm.IJMPIF labelLessThan] @
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd] @
                                [Asm.ILAB labelLessThan] @ // if e1 < e2 is true, evaluate e1>=e2 to false
                                [Asm.IPUSH 0]@
                                [Asm.ILAB labelEnd]

    // Logical operators AND, OR, and UNARY_MINUS
    | Syntax.AND (e1, e2)   ->  // Check e1. If e1 is false, push 0. If e1 is true, check e2. If e2 is false push 0, else push 1.
                                let labelTrue = newLabel()
                                let labelTrueSecond = newLabel()
                                let labelFalse = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IJMP labelFalse] @

                                [Asm.ILAB labelTrue] @
                                comp env e2 @
                                [Asm.IJMPIF labelTrueSecond] @
                                [Asm.IJMP labelFalse] @

                                [Asm.ILAB labelTrueSecond] @
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelFalse] @
                                [Asm.IPUSH 0] @
                                [Asm.IJMP labelEnd]@

                                [Asm.ILAB labelEnd] 
    | Syntax.OR (e1, e2)     -> // If e1 is true push 1, else evaluate e2, if e2 is true push 1. Else push 0.
                                let labelTrue = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @                     
                                [Asm.IJMPIF labelTrue] @          
                                comp env e2 @                      
                                [Asm.IJMPIF labelTrue] @          
                                [Asm.IPUSH 0] @                    
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelTrue] @            
                                [Asm.IPUSH 1]  @
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelEnd]
    | Syntax.UNARY_MINUS e1  -> // Evaluate e1, if true push 0, else push 1
                                let labelTrue = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd] @
                                [Asm.ILAB labelTrue] @
                                [Asm.IPUSH 0] @
                                [Asm.ILAB labelEnd] 
    // Function call handling
    | Syntax.CALL (f, [e1])  -> comp env e1 @       // Push (one) argument
                                [Asm.ICALL f] @
                                [Asm.ISWAP] @       // Remove (that) argument again
                                [Asm.IPOP]
    // LET and IF expressions
    | Syntax.LET (x, e1, e2) -> comp env e1 @
                                comp (extend env x) e2 @
                                [Asm.ISWAP ] @
                                [Asm.IPOP ]
    | Syntax.IF (e1, e2, e3) -> let labelThen = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                [Asm.IJMPIF labelThen] @
                                comp ("" :: env) e3 @
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelThen] @
                                comp ("" :: env) e2 @

                                [Asm.ILAB labelEnd]                        
    // Input and output operations
    | Syntax.WRITE e1       ->  comp env e1 @
                                [Asm.IWRITE]
    | Syntax.READ           ->  [Asm.IREAD] 

// Compiling a program by first compiling the list of function
// definitions and then compiling the "main" expression e1:
let rec compProg = function
    | ([], e1) ->
        comp [] e1 @
        [Asm.IHALT]
        
    | ((f, ([x1], e)) :: funcs, e1) ->
        compProg (funcs, e1) @
        [Asm.ILAB f] @
        comp [""; x1] e @ // Expect return address and (one) variable (x) on stack
        [Asm.ISWAP] @
        [Asm.IRETN]
   

