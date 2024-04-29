module Syntax

// This file defines the different types of expressions and program elements that
// exist in our tiny language. It essentially creates a blueprint for the AST.
// Each type in the code corresponds to a node in the AST. This file clarifies:
// - What kind of expressions are allowed
// - How expressions can be combined
// - The overall structure of a program

type funcname = string                      // function name
type varname  = string                      // variable name
type exp      = INT   of int                // integer literal
              | BOOL  of bool               // boolean literal
              | VAR   of string             // variable reference with its name
              | ADD   of exp * exp          
              | SUB   of exp * exp
              | MUL   of exp * exp
              | DIV   of exp * exp
              | MOD   of exp * exp
              | EQ    of exp * exp
              | NEQ   of exp * exp
              | LT    of exp * exp
              | LTE   of exp * exp
              | GT    of exp * exp
              | GTE   of exp * exp
              | CALL  of string * exp list 
              | AND   of exp * exp   (* && *)
              | OR    of exp * exp   (* || *)
              | UNARY_MINUS of exp  (* Unary Minus *)
              | LET   of varname * exp * exp (* let *)
              | IF    of exp * exp * exp    (* if then else *)
              | READ                         (* read *)
              | WRITE of exp                 (* write *)
type funcdef  = funcname * (varname list * exp)
type program  = funcdef list * exp

