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
    |ASTFunT (ts, ret) -> 
      fprintf fmt "([%a],%a)" pp_types ts pp_type ret
 
and pp_types fmt ts = pp_lst_cma pp_type fmt ts

let rec pp_arg fmt arg = 
  match arg with 
    ASTArg(id, ty) -> fprintf fmt "%s:%a" id pp_type ty 

and pp_args fmt args = pp_lst_cma pp_arg fmt args

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

and pp_exprs fmt es = pp_lst_cma pp_expr fmt es

let rec pp_stat fmt s =
  match s with
    ASTEcho e -> fprintf fmt "echo(%a)" pp_expr e

and pp_cmd fmt cmd = 
  match cmd with
    ASTStat s -> fprintf fmt "end(%a)" pp_stat s
    |ASTDef d -> pp_def fmt d

and pp_cmds fmt cmds =
  match cmds with
    [] -> ()
    | c :: cmds ->
        fprintf fmt "defs(%a, %a)" pp_cmd c pp_cmds cmds 

(* TODO: vérifie que c'est pas mauvais d'avoir en id String et pas ASTid*)
and pp_def fmt def = 
  match def with 
      ASTConst(id, ty, e) -> 
        fprintf fmt "const(%s,%a,%a)" id pp_type ty pp_expr e
    | ASTFun(id, ty, args, e)-> 
        fprintf fmt "fun(%s,%a,[%a],%a)" id pp_type ty pp_args args pp_expr e
    | ASTFunREC(id, ty, args, e)->
        fprintf fmt "fun_rec(%s,%a,[%a],%a)" id pp_type ty pp_args args pp_expr e

and pp_defs fmt defs = pp_lst_cma pp_def fmt defs

let pp_prog fmt = function
  ASTProg p -> fprintf fmt "prog(%a).\n" pp_cmds p




