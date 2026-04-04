open Types
open Ast
module StringMap = Map.Make(String)
module AdressMap = Map.Make(AdressOrd)

let pi1 = StringMap.of_list [("not", fun n -> if n = 1 then 0 else 1);]
let pi2 = StringMap.of_list [("eq", fun n1 n2 -> if n1 = n2 then 1 else 0);
                              ("lt",  fun n1 n2 -> if n1 < n2 then 1 else 0);
                              ("add", fun n1 n2 -> n1 + n2);
                              ("sub", fun n1 n2 -> n1 - n2);
                              ("mul", fun n1 n2 -> n1 * n2);
                              ("div", fun n1 n2 -> n1 / n2);
                              ]
let init_env = [("true", InZ 1); ("false", InZ 0)]
let init_memory = AdressMap.empty

type memory = memory_value AdressMap.t

(** Génère une adresse frèche et la renvoi avec la mémoire dans laquelle on associe cette adresse à Any*)
let alloc (mem: memory): adress * memory = 
  let fresh_add = AdressMap.cardinal mem in
  fresh_add, AdressMap.add fresh_add Any mem

let rec eval_prog: prog -> memory * output = function
    ASTProg cs -> 
      let mem_res, out_res = eval_block init_env init_memory []  cs in
      mem_res, List.rev out_res

and eval_block (env: environement) (mem: memory) (out: output): block -> memory * output = function
  | cs -> eval_cmds env mem out cs

 and eval_cmds (env: environement) (mem: memory) (out: output): cmd list -> memory * output = function
    (*END*)
    | [] -> mem, out
    (*STATS*)
    | ASTStat(s) :: cmds -> 
      let mem', out' = eval_stat env mem out s in
      eval_cmds env mem' out' cmds
    (*DEFS*)
    | ASTDef(d) :: cmds -> 
      let env', mem' = eval_def env mem d in
      eval_cmds env' mem' out cmds

and eval_stat (env: environement) (mem: memory) (out: output): stat ->  memory * output = function
    ASTEcho e -> 
      (* Pattern is exaustif, typer post cond*)
      begin
        match eval_expr env mem e with
          | InZ i -> mem, i :: out
          | _ -> failwith "impossible: expected InZ"
      end

    | ASTSet(id, e) -> 
        let _, v_add = List.find (fun (x, _) -> id = x) env in
          begin
            match v_add with 
              | InA a -> 
                  let v = eval_expr env mem e in
                  begin
                    match v with 
                    | InZ n -> AdressMap.add a (Current n) mem, out
                    |_ -> failwith (Printf.sprintf "Set: expression should evaluate to InZ for id %s " id)
                  end
                  
              | _ -> failwith (Printf.sprintf "Set applied on a constant %s " id)
          end

    | ASTIfStat(e, bk1, bk2) ->    
      let cond_i = Helper.eval_expr_for_InZ eval_expr env mem e "IfStat" in

      if cond_i = 1 then 
        eval_block env mem out bk1
      else 
        eval_block env mem out bk2
  
    | ASTWhile(e, bk) -> 
      let cond_i = Helper.eval_expr_for_InZ eval_expr env mem e "While" in

      if cond_i = 1 then 
        let mem', out' = eval_block env mem out bk in
        (* effet de bord est attendu sur e pour que la boucle termine*)
        eval_stat env mem' out' (ASTWhile(e, bk))
      else mem, out

    | ASTCall(e, es) -> 
            (*APP et APPR*)
      let vp = eval_expr env mem e in
      let vs = List.map (eval_expr env mem) es in
      begin match vp with
        | InP (bk, params, env') ->
            eval_block (Helper.bind env' params vs) mem out bk

        | InPR (bk, p_name, params, env') as self ->
            eval_block ((p_name, self) :: Helper.bind env' params vs) mem out bk

        | _ ->
            failwith "app on a non fonctionnel value"
      end

and eval_def (env: environement) (mem: memory): def -> environement * memory = function
    ASTConst (id, _, e) ->
        let v = eval_expr env mem e in
        ((id, v)::env), mem

    |ASTFun (id, _, args, e_body) ->
      (* Pour plus de lisibilité mettre un constructer de type ? pour closure*)
      (id, InF(e_body, List.map (function ASTArg(ident, _) -> ident) args, env))
      ::env, 
      mem

    |ASTFunREC (id, _, args, e_body) -> 
      (id, InFR(e_body, id, List.map (function ASTArg(ident, _) -> ident) args, env))
      ::env,
      mem

    |ASTVar (id, _) -> 
      let (fresh_add, new_mem) = alloc mem in
      ((id, InA fresh_add)::env), new_mem

    |ASTProc(id, args, bk) ->
      (id, InP(bk, List.map (function ASTArg(ident, _) -> ident) args, env))
      ::env,
      mem
    |ASTProcREC(id, args, bk) ->
      (id, InPR(bk, id, List.map (function ASTArg(ident, _) -> ident) args, env))
      ::env,
      mem

and eval_expr (env: environement) (mem: memory) (e:expr) : value = 

    match e with 
    | ASTNum n ->
      (*Note: pour la section 'Fonctions sémantiques utiles' des notes de cours APS0.
      Ici la conversion est faite par le lexer (int_of_string)*)
        InZ n

    | ASTId x ->
        let _, v = List.find (fun (id, _) -> id = x) env in
        begin
          match v with 
            | InA a -> 
              begin
                match AdressMap.find a mem with 
                  | Current n -> InZ n
                  | Any -> failwith "Acces of non initialized memory"
              end
            | _ -> v
        end
        
    | ASTIf (e1, e2, e3) ->
      let cond_i = Helper.eval_expr_for_InZ eval_expr env mem e1 "functionnal if" in

      if cond_i = 1 then 
        eval_expr env mem e2
      else 
        eval_expr env mem e3

    | ASTAnd (e1, e2) ->
      let i1 = Helper.eval_expr_for_InZ eval_expr env mem e1 "And (e1)" in

      if i1 = 1 then
        let i2 = Helper.eval_expr_for_InZ eval_expr env mem e2 "And (e2)" in
        InZ i2
      else
        InZ i1

    | ASTOr (e1, e2) ->
      let i1 = Helper.eval_expr_for_InZ eval_expr env mem e1 "Or (e1)" in

      if i1 = 1 then
        InZ i1
      else
        let i2 = Helper.eval_expr_for_InZ eval_expr env mem e2 "Or (e2)" in
        InZ i2

    | ASTApp (ASTId f, es) when StringMap.mem f pi1 || StringMap.mem f pi2 ->
      (*PRIM1 et PRIM2*)
      (*TODO: regarde c'est quoi la regle PRIM*)
      let vs = List.map (eval_expr env mem) es in
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
      (*APP et APPR*)
      let vf = eval_expr env mem e in
      let vs = List.map (eval_expr env mem) es in
      begin match vf with
      | InF (e_body, params, env') ->
          eval_expr (Helper.bind env' params vs) mem e_body

      | InFR (e_body, f_name, params, env') as self ->
          eval_expr ((f_name, self) :: Helper.bind env' params vs) mem e_body

      | _ ->
          failwith "app on a non fonctionnel value"
      end
      

    | ASTLambda (args, e_body) ->
        InF(e_body, List.map (function ASTArg (ident, _) -> ident) args, env)    
