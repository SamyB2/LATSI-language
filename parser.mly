%{
    open Latsi
%}
(* Tokens de ponctuation *)
%token EOF , CR , VIRG

(* Tokens de mots clés *)
%token IMPRIME , SI , ALORS , SINON , VAVERS , ENTREE 
%token MIN , MAX , FIN , REM , NL

(* Tokens d'opérateurs *)
%token PLUS , MOINS , MULT , DIV , EQ, RPAREN, LPAREN
%token AND , OR , NOT
%token <string>RELOP

(* Tokens de valeurs *)
%token <string> STRING 
%token <string> VAR 
%token <int> NOMBRE

%start <Latsi.lignes> input

(* Priorités et associativités *)
%left PLUS MOINS
%left MULT DIV
%left OR
%left AND
%nonassoc NOT
%nonassoc ALORS
%nonassoc SINON

%%
(* Régles de la grammaire *)

input: 
  | list_lignes=list(ligne) EOF { list_lignes }

ligne:
  | nbInstr=NOMBRE inst=instruction CR{(nbInstr, inst)}

instruction:
  (* Affichage *)
  | IMPRIME l=separated_nonempty_list(VIRG, explist){ Imp l }

  (* Expression conditionnelle *)
  | SI cond=condition ALORS i1=instruction SINON i2=instruction {Si (cond, i1 , Some i2)} 
  | SI cond=condition ALORS i1=instruction {Si (cond, i1 ,None )}

  (* Saut d'instruction *)
  | VAVERS e=expression{Vavers e}

  (* Entrée de données *)
  | ENTREE v=varlist {Entree v}

  (* Affectation de variables *)
  | l=separated_nonempty_list(VIRG, assign) {Assign l}

  (* Fin de programme *)
  | FIN {Fin}

  (* Commentaire *)
  | REM s=STRING {Rem s}

  (* Saut de ligne *)
  | NL { NL }

explist:
  | s=STRING{Str s}
  | e=expression{Expr e}

assign:
  | v=VAR EQ e=expression {v,e}

condition:
  | e1=expression re=RELOP e2=expression {C (e1, str_to_relop re, e2)}
  | e1=expression EQ e2=expression {C (e1, Equals , e2)}
  | NOT c=condition {NOT c}
  | c1=condition AND c2=condition {AND (c1, c2)}
  | c1=condition OR c2=condition {OR (c1, c2)}

varlist:
  | l=separated_nonempty_list(VIRG, VAR) {str_list_to_char l}

expression:
  | t=term {E t}
  | PLUS t=term {E t}
  | MOINS t=term {NEG (E t)}
  | t1=expression PLUS t2=expression {PLUS (t1, t2)}
  | t1=expression MOINS t2=expression {MOINS (t1, t2)}  

  (* Fonctions MIN et MAX *)
  | MIN LPAREN l=separated_nonempty_list(VIRG, expression) RPAREN {MIN l}
  | MAX LPAREN l=separated_nonempty_list(VIRG, expression) RPAREN {MAX l}

term:
  | f=facteur {T f}
  | t1=term MULT t2=term {MULT (t1, t2)}
  | t1=term DIV t2=term {DIV (t1, t2)}

facteur:
  | v=VAR {Var v}
  | n=NOMBRE {Nombre n}
  | LPAREN n=expression RPAREN {Exp n}