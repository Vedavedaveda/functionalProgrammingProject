module Interpreter

type varname = string
type funcname = string

type exp =
    | INT of int            
    | BOOL of bool 
    | VAR of varname         
    | ADD of exp * exp      
    | SUB of exp * exp      
    | MUL of exp * exp      
    | DIV of exp * exp      
    | MOD of exp * exp      
    | EQ of exp * exp       
    | NEQ of exp * exp      
    | LT of exp * exp       
    | LTE of exp * exp      
    | GT of exp * exp       
    | GTE of exp * exp      
    | AND of exp * exp      
    | OR of exp * exp       
    | UNARY_MINUS of exp    
    | IF of exp * exp * exp 
    | CALL of funcname * exp

type func = funcname * (varname * exp)

let isInteger = function
    | INT _ -> true
    | _ -> false

let isBoolean = function
    | BOOL _ -> true
    | _ -> false

let rec lookup x = function
    | []                -> failwith ("unbound: " + x)
    | (y, w) :: rest    -> if x = y then w else lookup x rest

let evalProg (funcs, e) = 
    let rec eval env = function
        | INT i -> i
        | BOOL b -> if b then 1 else 0
        | VAR x -> lookup x env
        // Arithmetic operators
        | ADD (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        v1 + v2
        | SUB (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        v1 - v2
        | MUL (e1, e2) ->
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        v1 * v2
        | DIV (e1, e2) ->
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        v1 / v2
        | MOD (e1, e2) ->
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        v1 % v2
        // Comparison operators
        | LT (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 < v2 then 1 else 0
        | GT (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 > v2 then 1 else 0
        | LTE (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 <= v2 then 1 else 0
        | GTE (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 >= v2 then 1 else 0
        | EQ (e1, e2) ->  
                        if not ((isInteger e1 && isInteger e2) || (isBoolean e1 && isBoolean e2)) then
                            failwith "Both expressions must be the same type."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 = v2 then 1 else 0
        | NEQ (e1, e2) ->  
                        if not ((isInteger e1 && isInteger e2) || (isBoolean e1 && isBoolean e2)) then
                            failwith "Both expressions must be the same type."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 <> v2 then 1 else 0
        // Logical Operators
        | AND (e1, e2) ->  
                        if not (isBoolean e1 && isBoolean e2) then
                            failwith "Both expressions must be boolean."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if ((v1 = 1) && (v2 = 1)) then 1 else 0
        | OR (e1, e2) ->  
                        if not (isBoolean e1 && isBoolean e2) then
                            failwith "Both expressions must be boolean."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if ((v1 = 1) || (v2 = 1)) then 1 else 0
        | UNARY_MINUS (e1) ->  
                        if not (isBoolean e1) then
                            failwith "Expression must be boolean."
                        let v1 = eval env e1
                        if (v1 = 1) then 1 else 0
        | IF (e1, e2, e3) ->
                        if not (isBoolean e1) then
                            failwith "Condition must be a boolean."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        let v3 = eval env e3
                        if v1 = 1 then v2 else v3
        | CALL (f, e) -> 
                        let v = eval env e
                        let (x, b) = lookup f funcs
                        eval [(x, v)] b
        | _ -> failwith "Not implemented"
    eval [] e

open Interpreter
evalProg([], ADD (INT 3, INT 3));;
evalProg([], SUB (INT 9, INT 3));;
evalProg([], MUL (INT 3, INT 3));;
evalProg([], DIV (INT 9, INT 3));;
evalProg([], MOD (INT 9, INT 5));;

evalProg([], LT (INT 9, INT 3));;
evalProg([], LT (INT 3, INT 9));;
evalProg([], LT (INT 9, INT 9));;
//evalProg([], LT (BOOL true, INT 9));;
//evalProg([], LT (BOOL true, BOOL true));;
//evalProg([], LT (INT 9, BOOL true));;

evalProg([], GT (INT 9, INT 3));;
evalProg([], GT (INT 3, INT 9));;
evalProg([], GT (INT 9, INT 9));;
//evalProg([], GT (BOOL true, INT 9));;
//evalProg([], GT (BOOL false, BOOL true));;
//evalProg([], GT (INT 9, BOOL false));;

evalProg([], LTE (INT 3, INT 9));;
evalProg([], LTE (INT 3, INT 3));;
evalProg([], LTE (INT 9, INT 3));;
//evalProg([], LTE (INT 9, BOOL false));;

evalProg([], GTE (INT 9, INT 3));;
evalProg([], GTE (INT 3, INT 3));;
evalProg([], GTE (INT 3, INT 9));;
//evalProg([], GTE (BOOL true, INT 3));;

evalProg([], EQ (INT 9, INT 9));;
evalProg([], EQ (INT 3, INT 9));;
evalProg([], EQ (BOOL true, BOOL false));;
evalProg([], EQ (BOOL true, BOOL true));;
evalProg([], EQ (BOOL false, BOOL false));;
//evalProg([], EQ (BOOL true, INT 9));;


evalProg([], NEQ (INT 9, INT 3));;
evalProg([], NEQ (INT 9, INT 9));;
evalProg([], NEQ (BOOL true, BOOL false));;
evalProg([], NEQ (BOOL true, BOOL true));;
evalProg([], NEQ (BOOL false, BOOL false));;
//evalProg([], NEQ (BOOL true, INT 9));;

evalProg ([], AND(BOOL false, BOOL true));;
evalProg ([], AND(BOOL true, BOOL true));;
evalProg ([], AND(BOOL false, BOOL false));;
//evalProg ([], AND(INT 5, BOOL false));;
//evalProg ([], AND(BOOL true, INT 5));;

evalProg ([], OR(BOOL false, BOOL true));;
evalProg ([], OR(BOOL true, BOOL true));;
evalProg ([], OR(BOOL false, BOOL false));;
//evalProg ([], OR(INT 5, BOOL false));;
//evalProg ([], OR(BOOL true, INT 5));;

evalProg ([], IF (BOOL false, INT 1, INT 5));;
evalProg ([], IF (BOOL true, INT 1, INT 5));;
//evalProg ([], IF (INT 5, BOOL true, INT 5));;