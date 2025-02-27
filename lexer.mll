{
    open Parser
    open String
    open Lexing
}

let ident_char = [ 'a'-'z' ]
let ident_char_cap = [ 'A'-'Z' ]
let chiffres = [ '0'-'9' ]
let layout = [' ' '\t' '\r']

let var = ident_char_cap
let nombre = chiffres chiffres*
let relop = '<'('>' | '=') | '>'('<' | '=' ) | '=' | '<' | '>'
let string = (',' | '`' | '\'' | '_'| ';' | ':' | '(' | ')' | '.' | ident_char | ident_char_cap | layout* )

rule token = parse
    (* Tokens pour la ponctuation *)
    | eof {EOF}
    |'\n' {CR}
    | ',' {VIRG}

    (* Tokens pour les mots clés *)
    | "IMPRIME" {IMPRIME}
    | "SI" {SI}
    | "ALORS" {ALORS}
    | "SINON" {SINON}
    | "VAVERS" {VAVERS}
    | "ENTREE" {ENTREE}
    | "FIN" {FIN}
    | "REM" {REM}
    | "NL" {NL}
    | "MIN" {MIN}
    | "MAX" {MAX}
    | "&&" {AND}
    | "||" {OR}
    | "!" {NOT} 

    (* Tokens pour les opérateurs *)
    | '+' {PLUS}
    | '-' {MOINS}
    | '*' {MULT}
    | '/' {DIV}
    | '=' {EQ}
    | relop {RELOP (lexeme lexbuf)}
    | ')'			{ RPAREN }
    | '('			{ LPAREN }

    (* Tokens pour les valeurs *)
    | '"' string* '"' as str {STRING (sub str 1 (length str - 2))}
    | var {VAR (lexeme lexbuf)}
    | nombre {NOMBRE (int_of_string(lexeme lexbuf))}

    (* Ignorer les espaces et les tabulations *)
    | layout+ {token lexbuf}

    (* Caractère inconnu *)
    | _ {failwith "Caractère inconnu"}
