%{
  open Ast
%}

%token EQ NEQ LE GE LT GT
%token CR EOF COMMA
%token ADD SUB MUL DIV MIN MAX LPAREN RPAREN NEG
%token PRINT IF THEN ELSE GOTO INPUT END REM NL ASSIGN
%token <int> NUM
%token <string> VAR
%token <string> STR

%start prog
%type <Ast.prog> prog

%right ADD SUB
%right MUL DIV
%right NEG
%nonassoc THEN
%nonassoc ELSE

%%

prog:
  | nonempty_list(line) EOF { List.rev $1 }

line:
  | NUM instr CR { ($1, $2) }
  | instr CR { (0, $1) }

instr:
  | PRINT separated_nonempty_list(COMMA, print) { Print $2 }
  | IF cond THEN instr %prec THEN { If ($2, $4, None) }
  | IF cond THEN instr ELSE instr { If ($2, $4, Some $6) }
  | GOTO expr { Goto $2 }
  | INPUT char_list { Input $2 }
  | ASSIGN separated_nonempty_list(COMMA, assign) { Assign $2 }
  | END { End }
  | REM VAR { Rem($2) }
  | NL { NL }

assign:
  | VAR EQ expr { ($1, $3) }

cond:
  | expr relop expr { ($1, $2, $3) }

char_list:
  | char_list COMMA VAR { $1 @ [String.get $3 0] }
  | VAR { [String.get $1 0] }

print:
  | STR { Str $1 }
  | expr { Exp $1 }

expr:
  | term { Term $1 }
  | NEG expr %prec NEG { Neg $2 }
  | expr ADD expr { Add ($1, $3) }
  | expr SUB expr { Sub ($1, $3) }
  | MIN LPAREN separated_nonempty_list(COMMA, expr) RPAREN { Min $3 }
  | MAX LPAREN separated_nonempty_list(COMMA, expr) RPAREN { Max $3 }

term:
  | facteur { Fact $1 }
  | term MUL term { Mult ($1, $3) }
  | term DIV term { Div ($1, $3) }

facteur:
  | VAR { Var $1 }
  | NUM { Num $1 }
  | LPAREN expr RPAREN { Expr $2 }

relop:
  | EQ { Eq }
  | NEQ { Neq }
  | GE { Ge }
  | GT { Gt }
  | LE { Le }
  | LT { Lt }

%%

let () =
  Printexc.record_backtrace true