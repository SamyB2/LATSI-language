
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | THEN
  | SUB
  | STR of (string)
  | RPAREN
  | REM
  | PRINT
  | NUM of (int)
  | NL
  | NEQ
  | NEG
  | MUL
  | MIN
  | MAX
  | LT
  | LPAREN
  | LE
  | INPUT
  | IF
  | GT
  | GOTO
  | GE
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | CR
  | COMMA
  | ASSIGN
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
