(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

(*TODO: ça a du sens de faire des déf recursives ?*)
type _type = 
   ASTBool
  | ASTInt
  | ASTFunT of _type list * _type

type arg = 
  ASTArg of string * _type

type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * expr list
  | ASTLambda of arg list * expr

type stat =
    ASTEcho of expr


type def = 
  ASTConst of string * _type * expr
  |ASTFun of string * _type * arg list * expr
  |ASTFunREC of string * _type * arg list * expr

type cmd =
  ASTStat of stat
  (* On peut avoir des imbrications de structures dans cmd !! l'absence est assuré par le parser*)
  (* En anticipation des prochaines versions de APS je met une structure
  même si j'aurais pu mettre une liste directement*)
  | ASTCmds of {defs: def list; last: cmd}

type prog = 
  ASTProg of cmd