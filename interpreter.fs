module Interpreter
type exp =
    | INT of int
    | VAR of string
    | BOOL of bool
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
    | IF of exp * exp * exp

let isInteger = function
    | INT _ -> true
    | _ -> false

let isBoolean = function
    | BOOL _ -> true
    | _ -> false

let evalProg (funcs, e) = 
    let rec eval env = function
        | INT i -> i
        | BOOL b -> if b then 1 else 0
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
        | EQ (e1, e2) ->  
                        if not (isInteger e1 && isInteger e2) then
                            failwith "Both expressions must be integers."
                        let v1 = eval env e1
                        let v2 = eval env e2
                        if v1 = v2 then 1 else 0
        | IF (e1, e2, e3) ->
                            if not (isBoolean e1) then
                                failwith "Condition must be a boolean."
                            let v1 = eval env e1
                            let v2 = eval env e2
                            let v3 = eval env e3
                            if v1 = 1 then v2 else v3

        | BOOL b -> if b then 1 else 0 // Evaluation of boolean literals
        | VAR s -> failwith "Variable evaluation not implemented"
        | _ -> failwith "Not implemented"  // Add other cases as needed
    eval [] e
open Interpreter
let result = evalProg ([], IF (BOOL false, INT 1, INT 5));;
// #load "interpreter.fs";;