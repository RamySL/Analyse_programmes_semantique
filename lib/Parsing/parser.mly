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
  LBRA cs=cmds RBRA        { cs }
;

cmds:
  st=stat                  { [ASTStat(st)] }
| ds=defs SEMCOL cs=cmds   { ASTCmds (ds, cs) }
;

def:
  CONST id=IDENT ty=_type e=expr { ASTConst(id, ty, e) } 
| FUN id=IDENT ty=_type LBRA as_=args RBRA e=expr { ASTFun(id, ty, as_, e) }
| FUN REC id=IDENT ty=_type LBRA as_=args RBRA e=expr { ASTFunREC(id, ty, as_, e) }
;

defs:
  d=def                     { [d] }
| d=def SEMCOL ds=defs      { d :: ds }
;

_type:
  BOOL                          { ASTBool }
| INT                           { ASTInt }
| LPAR ts=types ARROW rt=_type RPAR
    { ASTFunT(ts, rt) }
;

types:
  t=_type                     { [t] }
| t=_type STAR ts=types       { t :: ts }
;

arg:
  id=IDENT COL ty=_type        { ASTArg(id, ty) }
;

args:
  a=arg                        { [a] }
| a=arg COMMA as_=args         { a :: as_ }
;

stat:
  ECHO e=expr                  { ASTEcho(e) }
;

expr:
  n=NUM                         { ASTNum(n) }
| id=IDENT                      { ASTId(id) }
| LPAR IF c=expr t=expr f=expr RPAR { ASTIf(c, t, f) }
| LPAR AND a=expr b=expr RPAR { ASTAnd(a, b) } 
| LPAR OR a=expr b=expr RPAR { ASTOr(a, b) }
| LPAR fn=expr es=exprs RPAR { ASTApp(fn, es) }
| LBRA as_=args RBRA body=expr { ASTLambda(as_, body) }
;

exprs:
  e=expr                        { [e] }
| e=expr es=exprs               { e :: es }
;


%%