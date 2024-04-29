module Compiler

// Find the position of variable x on the runtime stack:
let rec varpos x = function
    | []       -> failwith ("unbound: " + x)
    | y :: env -> if x = y then 0 else 1 + varpos x env

// Adds new variables to the stack (aka: ectends the stack)
let extend env x = x :: env

// Generating fresh labels:
let mutable labelCounter = 0
let newLabel() =
    let this = labelCounter
    labelCounter <- this + 1;
    "L" + string(this)

// Compile an expression:
let rec comp env = function
    | Syntax.INT i           -> [Asm.IPUSH i]
    | Syntax.VAR x           -> [Asm.ILOAD (varpos x env)]
    | Syntax.BOOL b          -> if b then [Asm.IPUSH 1] else [Asm.IPUSH 0]
    | Syntax.ADD (e1, e2)    -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IADD]
    | Syntax.MUL (e1, e2)    -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IMUL]
    | Syntax.SUB (e1, e2)    -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ISUB]
    | Syntax.EQ (e1, e2)     -> comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ]
    | Syntax.DIV (e1, e2)   ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IDIV]
    | Syntax.MOD (e1, e2)   ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IMOD]
    // works
    | Syntax.NEQ (e1, e2)   ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ] @
                                [Asm.IPUSH 0] @
                                [Asm.IEQ]
    | Syntax.LT (e1, e2)    ->  comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ILT]
    // works
    | Syntax.LTE (e1, e2)   ->   // if less than is true, push 1; if it's false, check equality - if it's equal push 1, else push 0
                                let labelTrue = newLabel()
                                let labelFalse = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.ILT] @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IJMP labelFalse] @

                                [Asm.ILAB labelFalse] @ // if e1 == e2 is true, evaluate e1<=e2 to true, else to false
                                comp env e1 @
                                comp ("" :: env) e2 @
                                [Asm.IEQ] @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IPUSH 0] @
                                [Asm.IJMP labelEnd]@

                                [Asm.ILAB labelTrue] @ // if e1 < e2 is true, evaluate e1<=e2 to true
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd]@

                                [Asm.ILAB labelEnd]

    // works
    | Syntax.GT (e1, e2)    ->  let labelLessOrEqual= newLabel()
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
    // works
    | Syntax.GTE (e1, e2)   ->  let labelLessThan= newLabel()
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

    // works
    | Syntax.AND (e1, e2)   ->  let labelTrue = newLabel()
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


                                                               
    // works
    | Syntax.OR (e1, e2)     -> let labelTrue = newLabel()
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
    // works
    | Syntax.UNARY_MINUS e1  -> let labelTrue = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                [Asm.IJMPIF labelTrue] @
                                [Asm.IPUSH 1] @
                                [Asm.IJMP labelEnd] @
                                [Asm.ILAB labelTrue] @
                                [Asm.IPUSH 0] @
                                [Asm.ILAB labelEnd] 
    // CHECK
    | Syntax.CALL (f, [e1])  -> comp env e1 @ // Push (one) argument
                                [Asm.ICALL f] @
                                [Asm.ISWAP] @ // Remove (that) argument again
                                [Asm.IPOP]
    | Syntax.CALL (f, args)  -> 
                                let rec compileArgs = function
                                    | [] -> []
                                    | arg :: rest -> comp env arg @ compileArgs rest
                                compileArgs args @
                                [Asm.ICALL f] @
                                List.replicate (List.length args) Asm.IPOP

    | Syntax.LET (x, e1, e2) -> comp env e1 @
                                comp (extend env x) e2 @
                                [Asm.ISWAP ] @
                                [Asm.IPOP ]
    // works
    | Syntax.IF (e1, e2, e3) -> let labelThen = newLabel()
                                let labelEnd = newLabel()
                                comp env e1 @
                                [Asm.IJMPIF labelThen] @
                                comp ("" :: env) e3 @
                                [Asm.IJMP labelEnd] @

                                [Asm.ILAB labelThen] @
                                comp ("" :: env) e2 @

                                [Asm.ILAB labelEnd]
                                
    // CHECK
    | Syntax.WRITE e1       ->  comp env e1 @
                                [Asm.IWRITE] @
                                [Asm.IPUSH 1]

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

    | ((f, (args, e)) :: funcs, e1) ->
        let rec bindArgs env = function
            | [], _ -> env
            | arg :: rest, x -> bindArgs (extend env arg) (rest, x)
        
        let env = bindArgs [] (args, "") // Bind function arguments to local environment
        compProg (funcs, e1) @
        [Asm.ILAB f] @
        comp env e @
        [Asm.IRETN]

   

