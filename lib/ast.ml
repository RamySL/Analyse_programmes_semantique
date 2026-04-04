(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type _type = 
   ASTBool
  | ASTInt
  | ASTFunT of _type list * _type
  | ASTVoid

and arg = 
  ASTArg of string * _type

and expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * expr list
  | ASTLambda of arg list * expr

and stat =
      ASTEcho of expr
    | ASTSet of string * expr
    | ASTIfStat of expr * block * block
    | ASTWhile of expr * block
    | ASTCall of expr * expr list

and def = 
    ASTConst of string * _type * expr
  | ASTFun of string * _type * arg list * expr
  | ASTFunREC of string * _type * arg list * expr
  | ASTVar of string * _type
  | ASTProc of string * arg list * block
  | ASTProcREC of string * arg list * block


(* type interne *)
and cmd =
    ASTStat of stat
  | ASTDef of def

and block = cmd list

and prog = 
  ASTProg of block