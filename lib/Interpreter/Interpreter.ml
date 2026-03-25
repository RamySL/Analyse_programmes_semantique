(*
- On reçoit le programme qui a été donc typé, et du coup on a des préconditions qu'on peut prendre 
(ou bien des postscond fournit par le typeur)
*)

open Types
open Ast

module StringMap = Map.Make(String)

(*TODO: prq avoir pi1 séparé de pi2*)
let pi1 = StringMap.of_list [("not", fun n -> if n = 1 then 0 else 1);]
let pi2 = StringMap.of_list [("eq", fun n1 n2 -> if n1 = n2 then 1 else 0);
                              ("lt",  fun n1 n2 -> if n1 < n2 then 1 else 0);
                              ("add", fun n1 n2 -> n1 + n2);
                              ("sub", fun n1 n2 -> n1 - n2);
                              ("mul", fun n1 n2 -> n1 * n2);
                              ("div", fun n1 n2 -> n1 / n2);
                              ]
let init_env = [("true", InZ 1); ("false", InZ 0)]

let rec eval_prog: prog -> output = function
    ASTProg cs -> eval_cmds init_env [] cs

and eval_stat (env: environement) (out: output): stat -> output = function
    ASTEcho e -> 
      (* Pattern is exaustif, typer post cond*)
      (match eval_expr env e with
        | InZ i -> i :: out
        | _ -> failwith "impossible: expected InZ")


and eval_cmds (env: environement) (out: output): cmds -> output = function

    | [] -> out

    | ASTStat(s) :: cmds -> 
      let out' = eval_stat env out s in
      eval_cmds env out' cmds
    
    | ASTDef(d) :: cmds -> 
      let env' = eval_def env d in
      eval_cmds env' out cmds




(*TODO: factorise ce code*)
and eval_def (env: environement): def -> environement = function
    ASTConst (id, _, e) ->
        let v = eval_expr env e in
        ((id, v)::env)

    |ASTFun (id, _, args, e_body) ->
      (* Pour plus de lisibilité mettre un constructer de type ? pour closure*)
      (id, InF(e_body, List.map (function ASTArg(ident, _) -> ident) args, env))
      ::env

    |ASTFunREC (id, _, args, e_body) -> 
      (id, InFR(e_body, id, List.map (function ASTArg(ident, _) -> ident) args, env))
      ::env

and eval_expr (env: environement): expr -> value = function
      
    | ASTNum n ->
      (*Note: pour la section 'Fonctions sémantiques utiles' des notes de cours APS0.
      Ici la conversion est faite par le lexer (int_of_string)*)
        InZ n
    | ASTId x ->
        let _, v = List.find (fun (id, _) -> id = x) env in
        v
    | ASTIf (e1, e2, e3) ->
      (*TODO: clarification sur les litérales 'true' et 'false' dans expr*)
      (
        match eval_expr env e1 with
        | InZ iCond ->
            if iCond = 1 then eval_expr env e2
            else eval_expr env e3
        | _ -> failwith "impossible: expected InZ for condition"
      )
    | ASTAnd (e1, e2) ->
      (
        match eval_expr env e1 with
          | InZ i1 ->
              if i1 = 1 then
                (match eval_expr env e2 with
                | InZ i2 -> InZ i2
                | _ -> failwith "impossible: expected InZ for e2")
              else
                InZ i1
          | _ ->
              failwith "impossible: expected InZ for e1"
      )
    | ASTOr (e1, e2) ->
      (
        match eval_expr env e1 with
          | InZ i1 ->
              if i1 = 1 then
                InZ i1
              else
                (match eval_expr env e2 with
                | InZ i2 -> InZ i2
                | _ -> failwith "impossible: expected InZ for e2")
          | _ ->
              failwith "impossible: expected InZ for e1"
      )
    | ASTApp (e, es) ->
      (
      let vs = List.map(fun arg -> eval_expr env arg) es in
      (*TODO: e n'est pas eval rec, pb ?*)
      match e with
        ASTId fct_id ->
            (* Fonction primitives 
            (* TODO: peut être pas la meilleur manière de gérer ça*)
            *) 
          let nb_args = List.length es in
          (
            if nb_args = 1 then

              (
                match List.hd vs with
                  | InZ n ->
                      InZ ((StringMap.find fct_id pi1) n)
                  | _ ->
                      failwith "impossible: expected InZ for first argument"
              )

            else if nb_args = 2 then

              (match List.hd vs, List.nth vs 1 with
                | InZ n1, InZ n2 ->
                    InZ ((StringMap.find fct_id pi2) n1 n2)
                | _ ->
                    failwith "impossible: expected InZ, InZ for first two arguments"
              )
            else
              failwith "impossible: unsupported arity"
          )

        |_ ->(
          (*Fonctions utilisateurs*)
          let v_closure = eval_expr env e in
          let e_body, eval_body_env = 
            match v_closure with 
              InF (e_body, params, env') ->
                e_body, 
                (* On veut enchainer un List.map entre params et vs, pour avoir
                (param, valeur)list, puis on veut les rajouter donc avec un List.foldl
                avec comme accumulateur env'. ça on peut le faire avec fold_lfet2 *)

                List.fold_left2 (fun env_acc p v -> (p,v)::env_acc) (env') (params) (vs)
              |InFR (e_body, f_name, params, env') ->
                e_body,
                let env_partiel = List.fold_left2 (fun env_acc p v -> (p,v)::env_acc) (env') (params) (vs) in
                  (f_name, InFR (e_body, f_name, params, env')) :: env_partiel
          in
          eval_expr eval_body_env e_body
        )
      )

    | ASTLambda (args, e_body) ->
        InF(e_body, List.map (function ASTArg (ident, _) -> ident) args, env)    