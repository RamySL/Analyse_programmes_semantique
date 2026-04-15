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
  (*TODO: unused ?*)
  | ASTFunT of _type list * _type
  | ASTVoid
  | ASTVec of _type

and arg = 
  ASTArg of string * _type

  (* APS1a*)
and argP = 
  | ASTArg' of arg
  | ASTArgP of string * _type (* Argument passé par référence avec 'var'*)
 

and expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * expr list
  | ASTLambda of arg list * expr
  (* APS2 *)
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNth of expr * expr
  | ASTVset of expr * expr * expr

(* APS1a*)
and exprP = 
  | ASTexpr of expr
  | ASTAdr of string

and stat =
      ASTEcho of expr
    | ASTSet of lvalue * expr
    | ASTIfStat of expr * block * block
    | ASTWhile of expr * block
    | ASTCall of expr * exprP list

(* APS2 *)
(* On définit qd même un type lvalue même si c'est un sous type de 
expr pour contraindre la construction : ASTSet of lvalue * expr *)
and lvalue = 
  (* FIXME: ASTLVID de expr (sachant que ça sera un ASTId par construction du parser)*)
  | ASTLvId of string
  | ASTLvNth of lvalue * expr 

and def = 
    ASTConst of string * _type * expr
  | ASTFun of string * _type * arg list * expr
  | ASTFunREC of string * _type * arg list * expr
  | ASTVar of string * _type
  | ASTProc of string * argP list * block
  | ASTProcREC of string * argP list * block


(* type interne *)
and cmd =
    ASTStat of stat
  | ASTDef of def

and block = cmd list

and prog = 
  ASTProg of block