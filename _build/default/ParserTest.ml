(* ParserTest.ml *)

open Ast
open Printf

let print_prog prog =
  List.iter (fun (n, instr) -> printf "%d %s\n" n (string_of_instr instr)) prog

let print_position lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.Lexing.pos_lnum in
  let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  printf "Line %d, column %d: " line cnum

let parse_string input =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parser.prog Lexer.token lexbuf in
    print_prog result
  with
  | Parser.Error ->
      print_position lexbuf;
      let tok = Lexing.lexeme lexbuf in
      printf "Unexpected token '%s'\n" tok
  | Lexer.Error msg ->
      print_position lexbuf;
      printf "Lexer error: %s\n" msg

let () =
  let input = "10 IMPRIME \"Hello, World!\"\n20 SI 1 == 1 ALORS IMPRIME \"True\" SINON IMPRIME False\n30 FIN\n" in
  parse_string input
