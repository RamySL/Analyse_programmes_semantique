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
(* APS1 *)
%token VAR PROC SET IF_STAT WHILE CALL VOID
%token BOOL INT

%type <Ast.prog> prog

%start prog
(* un autre start avec liste de prédictions et modifier dans manipSys*)
%%

(*
  - separated_list(COMMA, option(expression)). Trouvé dans https://gallium.inria.fr/~fpottier/menhir/manual.pdf page 11
*)

prog: 
  b=block        { ASTProg b }
;

block:
  LBRA cs=cmds RBRA        { cs }

cmds:
  s=stat                    { [ASTStat s] }
| d=def SEMCOL cs=cmds      { ASTDef(d) :: cs  }
| s=stat SEMCOL cs=cmds     { ASTStat(s) :: cs  }
;

def:
  CONST id=IDENT ty=_type e=expr { ASTConst(id, ty, e) } 
| FUN id=IDENT ty=_type LBRA args=separated_list(COMMA, arg) RBRA e=expr { ASTFun(id, ty, args, e) }
| FUN REC id=IDENT ty=_type LBRA args=separated_list(COMMA, arg) RBRA e=expr { ASTFunREC(id, ty, args, e) }
(*APS1*)
| VAR id=IDENT ty=_type { ASTVar(id, ty) }
| PROC id=IDENT LBRA args=separated_list(COMMA, arg) RBRA blck=block { ASTProc(id, args, blck) }
| PROC REC id=IDENT LBRA args=separated_list(COMMA, arg) RBRA blck=block { ASTProcREC(id, args, blck) }
;

_type:
  BOOL                          { ASTBool }
| INT                           { ASTInt }
| VOID                          { ASTVoid }
| LPAR ts=separated_list(STAR, _type) ARROW rt=_type RPAR { ASTFunT(ts, rt) }
;


arg:
  id=IDENT COL ty=_type        { ASTArg(id, ty) }
;

stat:
  ECHO e=expr                  { ASTEcho(e) }
  (*APS1*)
  | SET id=IDENT e=expr        { ASTSet(id, e)}
  | IF_STAT e=expr b1=block b2=block  { ASTIfStat(e, b1, b2) }
  | WHILE e=expr b=block      { ASTWhile(e, b)}
  | CALL e=expr es=list(expr)     { ASTCall(e, es) }
;

expr:
  n=NUM                         { ASTNum(n) }
| id=IDENT                      { ASTId(id) }
| LPAR IF c=expr t=expr f=expr RPAR { ASTIf(c, t, f) }
| LPAR AND a=expr b=expr RPAR { ASTAnd(a, b) } 
| LPAR OR a=expr b=expr RPAR { ASTOr(a, b) }
| LPAR fn=expr es=list(expr) RPAR { ASTApp(fn, es) }
| LBRA args=separated_list(COMMA, arg) RBRA body=expr { ASTLambda(args, body) }
;

%%