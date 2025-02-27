(* open Ast

(* Exception pour les erreurs d'exécution *)
exception RuntimeError of string

let check_arguments () =
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: main <filename>.latsi";
    exit 1)

let check_file_exists filename =
  if not (Sys.file_exists filename) then (
    print_endline "Error: File does not exist";
    exit 1)

let check_file_suffix filename =
  if not (Filename.check_suffix filename ".latsi") then (
    print_endline "Error: Only .latsi files are allowed";
    exit 1)

let main =
  check_arguments ();
  let filename = Sys.argv.(1) in
  check_file_exists filename;
  check_file_suffix filename;
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Parser.prog Lexer.token lexbuf in
  eval_prog ast *)


open Ast
open Lexer
open Parser

let parse_program filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  try
    prog token lexbuf
  with
  | Lexer.Error msg ->
      Printf.eprintf "Lexical error: %s\n" msg;
      exit 1
  | Parser.Error ->
      Printf.eprintf "Syntax error\n";
      exit 1

let _ =
  let prog = parse_program Sys.argv.(1) in
  print_endline (str_of_prog prog);
  let env = eval_prog prog in
  print_endline env