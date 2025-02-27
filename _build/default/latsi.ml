open List

(* Les types OCaml pour la construction de l'arbre de syntaxe abstrait (ASA) *)
type lignes = (int * instruction) list 

and instruction =
  | Imp of expr_list_type list
  | Si of condition * instruction * (instruction option)
  | Vavers of expression
  | Entree of char list
  | Assign of (string * expression) list
  | Fin
  | Rem of string
  | NL

and expr_list_type =
  | Str of string
  | Expr of expression

and expression = 
  | E of term 
  | PLUS of expression * expression
  | MOINS of expression * expression
  | NEG of expression
  | MIN of expression list
  | MAX of expression list
  
and term = 
  | T of facteur
  | MULT of term * term
  | DIV of term * term 

and facteur =
  | Var of string
  | Nombre of int
  | Exp of expression

and relop =
  | Equals
  | GreaterThan
  | LessThan
  | GreaterThanOrEquals
  | LessThanOrEquals
  | Different

and condition = 
  | C of expression * relop * expression
  | AND of condition * condition
  | OR of condition * condition
  | NOT of condition

(* Fonction pour convertir l'ASA en string *)
(* as_string : (int * instruction) list -> string *)
let rec as_string lignes =
  match lignes with
  | [] -> ""
  | ligne :: rest -> 
    let nb ,instr = ligne in
    string_of_int nb ^ " " ^ as_string_inst instr ^ "\n" ^ as_string rest

(* as_string_inst : instruction -> string *)
and as_string_inst instruction  =
  match instruction with
  | Imp expr_list -> " " ^ as_string_expr_list expr_list
  | Si (c, i1, i2)-> 
    " SI " ^ as_string_condition c ^ " ALORS " ^ as_string_inst i1 ^ (
      match i2 with 
      | None -> ""
      | Some x -> " SINON " ^ as_string_inst x
    )
  | Vavers e -> " VAVERS " ^ as_string_expr e
  | Entree var_list -> " " ^ as_string_var_list var_list
  | Assign assing_list -> 
      let rec aux l acc = match l with 
        | [] -> acc
        | x::[] -> aux [] (acc ^ " , " ^ (fst x) ^ " = " ^ (as_string_expr (snd x)))
        | x::r -> aux r (acc ^ (fst x) ^ " = " ^ (as_string_expr (snd x)) ^ " , ")
      in aux assing_list "" 
  | Fin -> " FIN"
  | Rem s -> " REM " ^ s
  | NL -> " NL"

(* as_string_expr : expression -> string *)
and as_string_expr e =
  match e with
  | E t -> as_string_term t
  | PLUS (t1, t2) -> as_string_expr t1 ^ " + " ^ as_string_expr t2
  | MOINS (t1, t2) -> as_string_expr t1 ^ " - " ^ as_string_expr t2
  | NEG t -> "-" ^ as_string_expr t
  | MIN l -> "MIN " ^ (fold_left (fun acc x -> acc ^ as_string_expr x ^ " ") "" l)
  | MAX l -> "MAX " ^ (fold_left (fun acc x -> acc ^ as_string_expr x ^ " ") "" l)

(* as_string_term : term -> string *)
and as_string_term t = 
  match t with
  | T f -> as_string_fact f
  | MULT (f1, f2) -> as_string_term f1 ^ " * " ^ as_string_term f2
  | DIV (f1, f2) -> as_string_term f1 ^ " / " ^ as_string_term f2

(* as_string_fact : facteur -> string *)
and as_string_fact f =
  match f with
  | Var s -> s
  | Nombre n -> string_of_int n
  | Exp e -> as_string_expr e

(* as_string_var_list : char list -> string *)
and as_string_var_list v= 
  match v with 
  | [] -> ""
  | c :: reste -> (String.make 1 c) ^ as_string_var_list reste 

(* as_string_relop : relop -> string *)
and as_string_relop r =
  match r with
  | Equals -> "="
  | GreaterThan -> ">"
  | LessThan -> "<"
  | GreaterThanOrEquals -> ">="
  | LessThanOrEquals -> "<="
  | Different -> "<>"

(* as_string_expr_list : expr_list_type list -> string *)
and as_string_expr_list el = 
  match el with
  | [] -> ""
  | e :: reste ->  
  (match e with 
  | Str s -> s ^  " " ^ as_string_expr_list reste
  | Expr e1 -> as_string_expr e1 ^ " " ^ as_string_expr_list reste)

and as_string_condition c =
  match c with
  | C (e1, relo, e2) -> "("^as_string_expr e1 ^ " " ^ as_string_relop relo ^ " " ^ as_string_expr e2^")"
  | AND (c1, c2) -> "[" ^ as_string_condition c1 ^ ") AND (" ^ as_string_condition c2 ^ "]"
  | OR (c1, c2) -> "(" ^ as_string_condition c1 ^ ") OR (" ^ as_string_condition c2 ^ ")"
  | NOT c1 -> "NOT " ^ as_string_condition c1 ^ ")"

(* Fonction pour convertir une liste de string en liste composé du premier charactère *)
(* str_list_to_char : string list -> char list *)
let rec str_list_to_char c = 
  match c with 
  | [] -> []
  | ch :: r -> [ch.[0]] @ str_list_to_char r
  
(* Convertion de chaine en relop *)
(* str_to_relop : string -> relop *)
let str_to_relop s =
  if String.length s = 1 then
    if s.[0] = '=' then Equals 
    else if  s.[0] = '<' then LessThan
    else if s.[0] = '>' then GreaterThan
    else failwith "Unexpected char"
  else if String.length s = 2 then
    if (s.[0] = '=' && s.[1] ='<') || (s.[0] = '<' && s.[1] ='=') then LessThanOrEquals 
    else if (s.[0] = '=' && s.[1] ='>') || (s.[0] = '>' && s.[1] ='=') then GreaterThanOrEquals
    else if (s.[0] = '>' && s.[1] ='<') || (s.[0] = '<' && s.[1] ='>') then Different
    else failwith ("Unexpected string : '"^s^"'")
  else failwith ("Unexpected string : '"^s^"'") 

(* Évalue une instruction *)
(* eval_instr : instruction -> (string * int) Hashtbl -> int ref -> (int * instruction) array -> unit *)
let rec eval_instr i env instr_pointer prog =
  let is_edited = ref false in
  (match i with 
  | Imp expr_list -> iter (fun expr -> match expr with 
                            | Str s -> print_string s
                            | Expr e -> print_int (eval_expr e env)) expr_list;
                    

  | Si (c, i1 , i2) -> let tmp = ref (-1) in
    if eval_cond c env then eval_instr i1 env tmp prog
    else (match i2 with 
      | None -> ()
      | Some x -> eval_instr x env tmp prog);
    if !tmp = -1 then ()
    else (instr_pointer := !tmp ; is_edited := true)

  | Vavers e -> (instr_pointer := (index_of_instr prog (eval_expr e env));
                  is_edited := true)

  | Entree char_list -> 
    iter (fun c -> get_entry env (String.make 1 c)) char_list

  | Assign assing_list ->
    iter (fun x -> let s, e = x in 
                if not (Hashtbl.mem env s) 
                then Hashtbl.add env s (eval_expr e env) 
                else Hashtbl.replace env s (eval_expr e env)) assing_list

  | Fin -> (instr_pointer := Array.length prog ; is_edited := true) 

  | Rem _ -> ()

  | NL -> print_newline ()

  );

  if !instr_pointer = -1 then ()
  else
    if (!instr_pointer + 1) >= Array.length prog 
      then instr_pointer := Array.length prog
    else
      if not !is_edited 
      then (instr_pointer := !instr_pointer + 1;
            instr_pointer := get_next_instr prog (!instr_pointer + 1) 
                            (fst prog.(!instr_pointer)) !instr_pointer
            )    

(* eval_expr : expression -> (string * int) Hashtbl -> int *)
and eval_expr e env =
  match e with
  | E t -> eval_term t env
  | PLUS (e1, e2) -> eval_expr e1 env + eval_expr e2 env
  | MOINS (e1, e2) -> eval_expr e1 env - eval_expr e2 env
  | NEG e1 -> - (eval_expr e1 env)
  | MIN l -> fold_left (fun acc x -> min acc (eval_expr x env)) (eval_expr (hd l) env) (tl l)
  | MAX l -> fold_left (fun acc x -> max acc (eval_expr x env)) (eval_expr (hd l) env) (tl l)

(* eval_term : term -> (string * int) Hashtbl -> int *)
and eval_term t env =
  match t with
  | T f -> eval_fact f env
  | MULT (t1, t2) -> eval_term t1 env * eval_term t2 env
  | DIV (t1, t2) -> eval_term t1 env / eval_term t2 env

(* eval_fact : facteur -> (string * int) Hashtbl -> int *)
and eval_fact f env =
  match f with
  | Var s -> (try (Hashtbl.find env s) with Not_found -> failwith ("Variable "^s^" not found"))
  | Nombre n -> n 
  | Exp e -> eval_expr e env

(* Traduit l'operateur reçu en fonction *)
(* eval_relop : relop -> (int -> int -> bool) *)
and eval_relop r =
  match r with
  | Equals -> (=)
  | GreaterThan -> (>)
  | LessThan -> (<)
  | GreaterThanOrEquals -> (>=)
  | LessThanOrEquals -> (<=)
  | Different -> (<>)

(* Demande à l'utilisateur d'entrer un entier *)
(* get_entry : (string * int) Hashtbl -> string -> unit *)
and get_entry env var_id =
  try let entry = read_int () in Hashtbl.add env var_id entry
  with 
    | Failure _ -> (
        print_string "Erreur de saisie, veuillez entrer un entier : ";
        get_entry env var_id) 
    | End_of_file -> failwith "Impossible to read from stdin"

(* Renvoit le premier indice de la ligne dont le nombre est id *)
(* index_of_instr : (int * instruction) array -> int -> int *)
and index_of_instr prog id =
  let rec aux i = 
    if i = Array.length prog then failwith "Instruction not found"
    else if fst prog.(i) = id then i
    else aux (i+1)
  in aux 0

and eval_cond c env =
  match c with
  | C (e1, relo, e2) -> (eval_relop relo) (eval_expr e1 env) (eval_expr e2 env)
  | AND (c1, c2) -> (eval_cond c1 env) && (eval_cond c2 env)
  | OR (c1, c2) -> (eval_cond c1 env) || (eval_cond c2 env)
  | NOT c1 -> not (eval_cond c1 env)

and get_next_instr prog index id_ligne acc =
  if index >= Array.length prog then acc
  else
    if (fst prog.(index)) = id_ligne 
      then get_next_instr prog (index + 1) id_ligne index
    else acc