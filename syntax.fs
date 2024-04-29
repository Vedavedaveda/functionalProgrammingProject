// Define the Syntax module to encapsulate all syntax-related types
module Syntax

// This file defines the different types of expressions and program elements that
// exist in our tiny language. It essentially creates a blueprint for the AST.
// Each type in the code corresponds to a node in the AST. This file clarifies:
// - What kind of expressions are allowed
// - How expressions can be combined
// - The overall structure of a program

// Define types for function names and variable names for better readability
type funcname = string                      // function name
type varname  = string                      // variable name

// Define the 'exp' type representing all possible expressions in the language
type exp =
    | INT   of int                          // integer literal
    | BOOL  of bool                         // boolean literal
    | VAR   of string                       // represents a variable by its name
    | ADD   of exp * exp                    // addition
    | SUB   of exp * exp                    // subtraction
    | MUL   of exp * exp                    // multiplication
    | DIV   of exp * exp                    // division
    | MOD   of exp * exp                    // modulus
    | EQ    of exp * exp                    // equality comparison
    | NEQ   of exp * exp                    // inequality comparison
    | LT    of exp * exp                    // 'less than' comparison
    | LTE   of exp * exp                    // 'less than or equal to' comparison
    | GT    of exp * exp                    // 'greater than' comparison
    | GTE   of exp * exp                    // 'greater than or equal to' comparison
    | CALL  of string * exp list            // represents a function call with a name and a list of arguments
    | AND   of exp * exp                    // logical AND operation
    | OR    of exp * exp                    // logical OR operation
    | UNARY_MINUS of exp                    // unary minus operation
    | LET   of varname * exp * exp          // represents a 'let' binding for defining local variables
    | IF    of exp * exp * exp              // 'if-then-else' conditional expression
    | READ                                  // read operation for input
    | WRITE of exp                          // write operation for output

// Define the 'funcdef' type representing a function definition
type funcdef = funcname * (varname list * exp)  // function definition includes a name and a pair of parameters and body expression

// Define the 'program' type representing the entire program
type program = funcdef list * exp               // program consists of a list of function definitions and a single main expression
