 %{

(* ========================================================================== *)

(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)

(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)

(* == Analyse des programmes et sémantiques                                == *)

(* ========================================================================== *)

(* == Analyse syntaxique                                                   == *)

(* ========================================================================== *)


open Ast


%} 

%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token SEMCOL COL COMMA STAR ARROW
%token ECHO CONST FUN REC IF AND OR
%token BOOL INT

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

%start prog

%%

prog: 
  LBRA cmds RBRA        { $2 }
;

cmds:
  stat                  { [ASTStat($1)] }
| def SEMCOL cmds       { $1 :: $3 }
;

def:
  CONST IDENT _type expr                { ASTConst($2, $3, $4) } 
| FUN IDENT _type LBRA args RBRA expr   { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT _type LBRA args RBRA expr { ASTFunREC($3, $4, $6, $8) }
;

_type:
  BOOL                          { ASTBool }
| INT                           { ASTInt }
| LPAR types ARROW _type RPAR { ASTFunT($2, $4) }
;

types:
  _type                     { [$1] }
| _type STAR types          { $1 :: $3 }
;

arg:
  IDENT COL _type           { ASTArg($1, $3) }
;

args:
  arg                           { [$1] }
| arg COMMA args                { $1 :: $3 }
;

stat:
  ECHO expr                     { ASTEcho($2) }
;

expr:
  NUM                           { ASTNum($1) }
| IDENT                         { ASTId($1) }
| LPAR IF expr expr expr RPAR   { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR       { ASTAnd($3, $4) } 
| LPAR OR expr expr RPAR        { ASTOr($3, $4) }
| LPAR expr exprs RPAR          { ASTApp($2, $3) }
| LBRA args RBRA expr           { ASTLambda($2, $4) }
;

exprs:
  expr                          { [$1] }
| expr exprs                    { $1 :: $2 }
;

%%