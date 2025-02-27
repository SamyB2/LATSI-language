open List

type relop = Eq | Gt | Ge | Lt | Le | Neq

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Neg of expr
  | Min of expr list
  | Max of expr list
  | Term of term

and term = Fact of facteur | Mult of term * term | Div of term * term

and facteur = Var of string | Num of int | Expr of expr

type print = Str of string | Exp of expr

type instr =
  | If of (expr * relop * expr) * instr * instr option
  | Goto of expr
  | Print of print list
  | Input of char list
  | Assign of (string * expr) list
  | End
  | Rem of string
  | NL

type prog = (int * instr) list

let str_of_relop = function
  | Eq -> "=="
  | Neq -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

let rec string_of_expr = function
  | Add (e1, e2) -> string_of_expr e1 ^ " + " ^ string_of_expr e2
  | Sub (e1, e2) -> string_of_expr e1 ^ " - " ^ string_of_expr e2
  | Neg e -> "-" ^ string_of_expr e
  | Min l -> "MIN (" ^ String.concat ", " (map string_of_expr l) ^ ")"
  | Max l -> "MAX (" ^ String.concat ", " (map string_of_expr l) ^ ")"
  | Term t -> string_of_term t

and string_of_factor = function
  | Var v -> v
  | Num i -> string_of_int i
  | Expr e -> string_of_expr e

and string_of_term = function
  | Fact f -> string_of_factor f
  | Mult (t1, t2) -> string_of_term t1 ^ " * " ^ string_of_term t2
  | Div (t1, t2) -> string_of_term t1 ^ " / " ^ string_of_term t2

let string_of_print = function
  | Str s -> "\"" ^ s ^ "\""
  | Exp e -> string_of_expr e

let rec string_of_instr = function
  | Print l -> "IMPRIME " ^ String.concat ", " (map string_of_print l)
  | If ((e1, r1, e2), i1, i2) -> (
      "IF " ^ string_of_expr e1 ^ " " ^ str_of_relop r1 ^ " "
      ^ string_of_expr e2 ^ " THEN " ^ string_of_instr i1
      ^ match i2 with None -> "" | Some i -> " SINON " ^ string_of_instr i)
  | Goto e -> "VAVERS " ^ string_of_expr e
  | Input l -> "ENTREE " ^ String.concat ", " (map (String.make 1) l)
  | Assign l ->
      String.concat ", " (map (fun (s, e) -> s ^ " == " ^ string_of_expr e) l)
  | End -> "FIN"
  | Rem s -> "REM " ^ s
  | NL -> "NL"

let str_of_prog l =
  String.concat "\n"
    (map (fun (n, i) -> string_of_int n ^ " " ^ string_of_instr i) l)

(*  Evaluation *)

let rec eval_expr env = function
  | Add (e1, e2) -> eval_expr env e1 + eval_expr env e2
  | Sub (e1, e2) -> eval_expr env e1 - eval_expr env e2
  | Neg e -> -eval_expr env e
  | Min l -> fold_left min max_int (map (eval_expr env) l)
  | Max l -> fold_left max min_int (map (eval_expr env) l)
  | Term t -> eval_term env t

and eval_factor env = function
  | Var v -> assoc v env
  | Num i -> i
  | Expr e -> eval_expr env e

and eval_term env = function
  | Fact f -> eval_factor env f
  | Mult (t1, t2) -> eval_term env t1 * eval_term env t2
  | Div (t1, t2) -> eval_term env t1 / eval_term env t2

let eval_relop env e1 op e2 =
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  match op with
  | Eq -> v1 = v2
  | Neq -> v1 <> v2
  | Lt -> v1 < v2
  | Gt -> v1 > v2
  | Le -> v1 <= v2
  | Ge -> v1 >= v2

let print_args env args =
  iter
    (fun arg ->
      match arg with
      | Str s -> print_string s
      | Exp e ->
          print_int (eval_expr env e);
          print_string " ")
    args;
  print_newline ()

let rec eval_instr env = function
  | Print l ->
      print_args env l;
      Some (env, None)
  | If ((e1, op, e2), i1, i2) ->
      if eval_relop env e1 op e2 then
        match eval_instr env i1 with
        | None -> Some (env, None)
        | Some (env', next) ->
            Option.fold ~none:(Some (env', next)) ~some:(eval_instr env') i2
      else Some (env, None)
  | Goto e -> Some (env, Some (eval_expr env e))
  | Input l ->
      iter
        (fun c ->
          print_string (String.make 1 c);
          flush stdout)
        l;
      Some (env, None)
  | Assign l -> Some (map (fun (s, e) -> (s, eval_expr env e)) l, None)
  | End -> None
  | Rem _ -> Some (env, None)
  | NL ->
      print_newline ();
      Some (env, None)

let eval_prog p =
  let rec aux env = function
    | [] -> env
    | (_, i) :: tl -> (
        match eval_instr env i with
        | None -> env
        | Some (env', next) -> (
            match next with
            | None -> aux env' tl
            | Some e -> aux env' (filter (fun (n', _) -> n' = e) p)))
  in
  let final_env = aux [] p in
  let str_env =
    List.map (fun (var, value) -> var ^ " = " ^ string_of_int value) final_env
  in
  String.concat "\n" str_env
