
type ’a env = (varname * ’a) list
let rec lookup x = function
    | [] -> failwith ("unbound: " + x)
    | (y, w) :: env -> if x = y then w else lookup x env

// eval : int ienv -> exp -> int
let evalProg (funcs, e) = 
    let rec eval env = function
        //
        | Syntax.INT i           -> i 
        //
        | Syntax.VAR x           -> lookup x env 
        //
        | Syntax.ADD (e1, e2)    -> let v1 = eval env e1
                                    let v2 = eval env e2
                                    v1 + v2
        //
        | Syntax.EQ (e1, e2)     -> let v1 = eval env e1
                                    let v2 = eval env e2
                                    if v1 = v2 then 1 else 0
        //
        | Syntax.CALL (f, e)     -> let v = eval env e
                                    let (x, body) = lookup f funcs
                                    eval [(x, v)] body
        //
        | Syntax.LET (x, e1, e2) -> let v1 = eval env e1
                                    eval((x,v1) :: env) e2
eval [] e