{
  open Parser
  exception Error of string
  let keyword_or_ident = function
    | "IMPRIME" -> PRINT
    | "SI" -> IF
    | "ALORS" -> THEN
    | "SINON" -> ELSE
    | "VAVERS" -> GOTO
    | "ENTREE" -> INPUT
    | "FIN" -> END
    | "REM" -> REM
    | "NL" -> NL
    | "==" -> EQ
    | "!=" -> NEQ
    | "<=" -> LE
    | ">=" -> GE
    | "<" -> LT
    | ">" -> GT
    | _ as id -> VAR id
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }

  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<" { LT }
  | ">" { GT }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }

  | "MIN" { MIN }
  | "MAX" { MAX }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "NEG" { NEG }

  | "IMPRIME" { PRINT }
  | "SI" { IF }
  | "ALORS" { THEN }
  | "SINON" { ELSE }
  | "VAVERS" { GOTO }
  | "ENTREE" { INPUT }
  | "FIN" { END }
  | "REM" { REM }
  | "NL" { NL }
  | "=" { ASSIGN }

  | ['0'-'9']+ as num { NUM (int_of_string num) }
  | '"'[^'"']*'"' as str { STR (String.sub str 1 (String.length str - 2)) }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { keyword_or_ident id }
  | "\n" { CR }
  | eof { EOF }
  | _ as char { raise (Error (Printf.sprintf "Unknown character: %c" char)) }
