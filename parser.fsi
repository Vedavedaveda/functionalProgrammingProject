// Signature file for parser generated by fsyacc
module Parser
type token = 
  | TRUE
  | FALSE
  | WRITE
  | READ
  | EOF
  | FUNC
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | EQ
  | LPAR
  | RPAR
  | COMMA
  | SEMICOLON
  | ANDAND
  | BANGEQ
  | BARBAR
  | EQEQ
  | GT
  | GTEQ
  | LT
  | LTEQ
  | MINUS
  | PCT
  | PLUS
  | SLASH
  | STAR
  | NAME of (string)
  | INT of (int)
type tokenId = 
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_WRITE
    | TOKEN_READ
    | TOKEN_EOF
    | TOKEN_FUNC
    | TOKEN_LET
    | TOKEN_IN
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_EQ
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_COMMA
    | TOKEN_SEMICOLON
    | TOKEN_ANDAND
    | TOKEN_BANGEQ
    | TOKEN_BARBAR
    | TOKEN_EQEQ
    | TOKEN_GT
    | TOKEN_GTEQ
    | TOKEN_LT
    | TOKEN_LTEQ
    | TOKEN_MINUS
    | TOKEN_PCT
    | TOKEN_PLUS
    | TOKEN_SLASH
    | TOKEN_STAR
    | TOKEN_NAME
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_exp
    | NONTERM_exps
    | NONTERM_explist
    | NONTERM_names
    | NONTERM_namelist
    | NONTERM_def
    | NONTERM_defs
    | NONTERM_prog
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val prog : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Syntax.program) 
