open Array
open Latsi

let lexbuf = Lexing.from_channel stdin 

(* latsi : instruction list *)
let latsi = Parser.input Lexer.token lexbuf 

(* eval : (int * instruction) array -> (string * int) Hashtbl -> int ref unit *)
let rec eval prog env instr_pointer = 
  if !instr_pointer >= length prog then ()
  else (eval_instr (snd prog.(!instr_pointer)) env instr_pointer prog; 
    eval prog env instr_pointer)

let _ = Printf.printf "Evaluation : \n" ;
        if latsi = [] then print_string "Erreur : programme vide\n"
        else
        eval (of_list (List.sort (fun (i1, _) (i2, _) -> compare i1 i2) latsi))
         (Hashtbl.create 26) (ref 0) ;