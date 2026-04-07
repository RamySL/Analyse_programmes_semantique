(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
open Format

let sep_cma fmt () = fprintf fmt ", "
                    (* retourne une fct qui prend une fmt et une liste*)
let pp_lst_cma p = pp_print_list ~pp_sep:sep_cma p

let rec pp_type fmt t = 
  match t with 
    ASTBool -> fprintf fmt "bool"
    |ASTInt -> fprintf fmt "int"
    |ASTVoid -> fprintf fmt "void"
    |ASTFunT (ts, ret) -> 
      fprintf fmt "([%a],%a)" pp_types ts pp_type ret
    |ASTVec t -> fprintf fmt "vec(%a)" pp_type t
 
and pp_types fmt ts = pp_lst_cma pp_type fmt ts

let rec pp_arg fmt arg = 
  match arg with 
    ASTArg(id, ty) -> fprintf fmt "(%s,%a)" id pp_type ty 

and pp_args fmt args = pp_lst_cma pp_arg fmt args

let rec pp_argP fmt argP = 
  match argP with 
     ASTArg' arg -> pp_arg fmt arg
    |ASTArgP(id, ty) -> fprintf fmt "(var(%s),%a)" id pp_type ty (*déclaré avec préfixe : var*)

and pp_argPs fmt args = pp_lst_cma pp_argP fmt args

let rec pp_expr fmt e =
  match e with
  | ASTNum n ->
      fprintf fmt "num(%d)" n
  | ASTId x ->
      fprintf fmt "id(%s)" x
  | ASTIf (e1, e2, e3) ->
      fprintf fmt "if(%a,%a,%a)" pp_expr e1 pp_expr e2 pp_expr e3
  | ASTAnd (e1, e2) ->
      fprintf fmt "and(%a,%a)" pp_expr e1 pp_expr e2
  | ASTOr (e1, e2) ->
      fprintf fmt "or(%a,%a)" pp_expr e1 pp_expr e2
  | ASTApp (e, es) ->
      fprintf fmt "app(%a,[%a])" pp_expr e pp_exprs es
  | ASTLambda (args, body) ->
      fprintf fmt "abs([%a],%a)" pp_args args pp_expr body
    (* APS2 *)
  | ASTAlloc e ->
      fprintf fmt "alloc(%a)" pp_expr e
  | ASTLen e ->
      fprintf fmt "len(%a)" pp_expr e
  | ASTNth (e1, e2) ->
      fprintf fmt "nth(%a,%a)" pp_expr e1 pp_expr e2
  | ASTVset (e1, e2, e3) ->
      fprintf fmt "vset(%a,%a,%a)" pp_expr e1 pp_expr e2 pp_expr e3

and pp_exprs fmt es = pp_lst_cma pp_expr fmt es

let rec pp_exprP fmt ep = 
    match ep with 
    (* On wrap ?*)
    | ASTexpr e -> pp_expr fmt e
    | ASTAdr id -> fprintf fmt "adr(id(%s))" id 

and pp_exprPs fmt eps = pp_lst_cma pp_exprP fmt eps

let rec pp_stat fmt s =
  match s with
      ASTEcho e -> fprintf fmt "echo(%a)" pp_expr e
    | ASTSet(lv, e) -> fprintf fmt "set(%a, %a)" pp_lvalue lv pp_expr e
    | ASTIfStat(e, bk1, bk2) -> fprintf fmt "if_stat(%a,%a,%a)" pp_expr e pp_block bk1 pp_block bk2
    | ASTWhile(e, bk) -> fprintf fmt "while(%a,%a)" pp_expr e pp_block bk
    | ASTCall(e, eps) -> fprintf fmt "call(%a,[%a])" pp_expr e pp_exprPs eps

and pp_lvalue fmt lv = 
    match lv with 
        |ASTLvId id -> fprintf fmt "id(%s)" id
        |ASTLvNth(lv, e) -> fprintf fmt "nth(%a,%a)" pp_lvalue lv pp_expr e

and pp_cmds fmt cmds =
  match cmds with
    | [] -> 
        fprintf fmt "end"
    | ASTDef d :: cmds ->
        fprintf fmt "defs(%a, %a)" pp_def d pp_cmds cmds 
    | ASTStat s :: cmds ->
        fprintf fmt "stats(%a, %a)" pp_stat s pp_cmds cmds 

and pp_def fmt def = 
  match def with 
      ASTConst(id, ty, e) -> 
        fprintf fmt "const(%s,%a,%a)" id pp_type ty pp_expr e
    | ASTFun(id, ty, args, e)-> 
        fprintf fmt "fun(%s,%a,[%a],%a)" id pp_type ty pp_args args pp_expr e
    | ASTFunREC(id, ty, args, e)->
        fprintf fmt "fun_rec(%s,%a,[%a],%a)" id pp_type ty pp_args args pp_expr e
    | ASTVar (id, ty) -> 
        fprintf fmt "var(%s,%a)" id pp_type ty
    |ASTProc(p, args, bk) ->
        fprintf fmt "proc(%s,[%a],%a)" p pp_argPs args pp_block bk
    |ASTProcREC(p, args, bk) ->
        fprintf fmt "proc_rec(%s,[%a],%a)" p pp_argPs args pp_block bk
and pp_defs fmt defs = pp_lst_cma pp_def fmt defs

and pp_block fmt (blck: cmd list) = 
    fprintf fmt "block(%a)" pp_cmds blck

let pp_prog fmt = function
  ASTProg p -> fprintf fmt "prog(%a).\n" pp_block p




