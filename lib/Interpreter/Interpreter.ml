(*
- On reçoit le programme qui a été donc typé, et du coup on a des préconditions qu'on peut prendre 
(ou bien des postscond fournit par le typeur)
*)

open Types
open Ast

(*TODO: propage la modif de l'ast avec prog*)
(*TODO: Il faut introduire PI1 et PI2 les envs des prémitives*)

let rec eval_prog (p: prog):output = function
    (*TODO: ici on init les env à propager*)
    ASTProg cs -> eval_cmd cs

and eval_cmd (cs: cmd) (env: environement) (out: output): output = function

    ASTStat (ASTEcho e) ->
      let InZ i = eval_expr e env in
      i::out
    |ASTCmds {defs=[]; last} -> 
      eval_cmd last env out
    |ASTCmds {defs=d::ds; last} -> 
      let env' = eval_def d env in
      eval_cmd (ASTCmds {defs=ds; last}) env' out


(*TODO: factorise ce code*)
and eval_def (d: def) (env: environement): environement = function
    ASTConst (id, _, e) ->
        let v = eval_expr e env in
        ((id, v)::env)

    |ASTFun (id, _, args, e) ->
      (* Pour plus de lisibilité mettre un constructer de type ? pour closure*)
      (id, InF(e, List.map (fun (ident, _) -> ident) args, env))
      ::env

    |ASTFunREC (id, _, args, e) -> 
      (id, InFR(e, id, List.map (fun (ident, _) -> ident) args, env))
      ::env

and eval_expr (e: expr) (env: environement): value = function
      
    | ASTNum n ->
      (*Note: pour la section 'Fonctions sémantiques utiles' des notes de cours APS0.
      Ici la conversion est faite par le lexer (int_of_string)*)
        InZ n
    | ASTId x ->
        let _, v = List.find (fun (id, v) -> id = x) env in
        v
    | ASTIf (e1, e2, e3) ->
      (*TODO: clarification sur les litérales 'true' et 'false' dans expr*)
        let InZ iCond = eval_expr e1 in
        let v = 
          if iCond=1 then eval_expr e2 env
          else eval_expr e3 env
        in
        v
    | ASTAnd (e1, e2) ->
        fprintf fmt "and(%a,%a)" pp_expr e1 pp_expr e2
    | ASTOr (e1, e2) ->
        fprintf fmt "or(%a,%a)" pp_expr e1 pp_expr e2
    | ASTApp (e, es) ->
        fprintf fmt "app(%a,[%a])" pp_expr e pp_exprs es
    | ASTLambda (args, body) ->
        fprintf fmt "abs([%a],%a)" pp_args args pp_expr body

