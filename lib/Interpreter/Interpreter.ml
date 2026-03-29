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
      begin
        match eval_expr env e with
          | InZ i -> i :: out
          | _ -> failwith "impossible: expected InZ"
      end


and eval_cmds (env: environement) (out: output): cmds -> output = function

    | [] -> out

    | ASTStat(s) :: cmds -> 
      let out' = eval_stat env out s in
      eval_cmds env out' cmds
    
    | ASTDef(d) :: cmds -> 
      let env' = eval_def env d in
      eval_cmds env' out cmds


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


and eval_expr (env: environement) (e:expr) : value = 

    (*Retourne l'environement env etendu avec tous les binding formé par les éléments
    de params et les valeurs de vs*)
    let bind (env:environement) params vs : environement =
      if List.length params <> List.length vs then
        failwith "arity mismatch"
      else
        List.fold_left2 (fun acc p v -> (p, v) :: acc) env params vs
    in

    match e with 
    | ASTNum n ->
      (*Note: pour la section 'Fonctions sémantiques utiles' des notes de cours APS0.
      Ici la conversion est faite par le lexer (int_of_string)*)
        InZ n
    | ASTId x ->
        let _, v = List.find (fun (id, _) -> id = x) env in
        v
    | ASTIf (e1, e2, e3) ->
      begin
        match eval_expr env e1 with
        | InZ iCond ->
            if iCond = 1 then eval_expr env e2
            else eval_expr env e3
        | _ -> failwith "impossible: expected InZ for condition"
      end
    | ASTAnd (e1, e2) ->
      begin
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
      end
    | ASTOr (e1, e2) ->
      begin
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
      end

    | ASTApp (ASTId f, es) when StringMap.mem f pi1 || StringMap.mem f pi2 ->
      let vs = List.map (eval_expr env) es in
      begin match f, vs with
        | "not", [InZ n] ->
            InZ ((StringMap.find f pi1) n)
        | ("eq" | "lt" | "add" | "sub" | "mul" | "div"), [InZ n1; InZ n2] ->
            InZ ((StringMap.find f pi2) n1 n2)
        | _ ->
            let l = List.map (fun v -> Printf.sprintf "%s" (Helper.string_of_value v) ) vs in
            failwith (Printf.sprintf "primitive : %s applied erroneously on : %s" f (String.concat " " l))
      end

    | ASTApp (e, es) ->
      let vf = eval_expr env e in
      let vs = List.map (eval_expr env) es in
      begin match vf with
      | InF (e_body, params, env') ->
          eval_expr (bind env' params vs) e_body

      | InFR (e_body, f_name, params, env') as self ->
          eval_expr ((f_name, self) :: bind env' params vs) e_body

      | _ ->
          failwith "app on a non fonctionnel value"
      end
      

    | ASTLambda (args, e_body) ->
        InF(e_body, List.map (function ASTArg (ident, _) -> ident) args, env)    

