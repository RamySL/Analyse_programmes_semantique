open Types
open Ast
module StringMap = Map.Make(String)

let pi1 = StringMap.of_list [("not", fun n -> if n = 1 then 0 else 1);]
let pi2 = StringMap.of_list [("eq", fun n1 n2 -> if n1 = n2 then 1 else 0);
                              ("lt",  fun n1 n2 -> if n1 < n2 then 1 else 0);
                              ("add", fun n1 n2 -> n1 + n2);
                              ("sub", fun n1 n2 -> n1 - n2);
                              ("mul", fun n1 n2 -> n1 * n2);
                              ("div", fun n1 n2 -> n1 / n2);
                              ]
let init_env = [("true", InZ 1); ("false", InZ 0)]

(* len est le nombre de valeurs stockées *)
(* TODO: abstrait le fait que tu utilise un array (avec des accesseurs) *)
type memory = { memory : memory_value array; len: int }

let init_memory = { memory=Array.make 1024 Any; len=0 } 

(** Allou 'size' cases mémoire, retourne la premiere adresse du bloque alloué.*)
let allocn (mem : memory) (size : int) : adress * memory =
  if size < 0 then
    failwith "allocn: negative size"
  else
    let rec ensure_capacity (mem : memory) : memory =
      let capacity = Array.length mem.memory in
      if mem.len + size <= capacity then
        mem
      else
        let new_mem_arr =
          (* On a new = 2 * prev en taille*)
          Array.append mem.memory (Array.make (capacity) Any)
        in
        ensure_capacity { mem with memory = new_mem_arr }
    in

    let mem = ensure_capacity mem in
    let fresh_add = mem.len in
    (fresh_add, { mem with len = mem.len + size })

let alloc (mem : memory) : adress * memory =
  allocn mem 1

(***********  MAIN CORE OF EVALUATION ****************************************)
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

and eval_lvalue (env: environement) (mem: memory): lvalue -> adress * memory = function

    | ASTLvId id -> 
      let a = Helper.match_value_for_InA (snd (List.find (fun (id',_) -> id'=id) env)) in
      a, mem

    |ASTLvNth (ASTLvId id, e) -> 
      let ve, mem' = eval_expr env mem e in
      let i = Helper.match_value_for_InZ ve "ASTLVNeth(ve)" in
      let adr, size = Helper.match_value_for_InBlock (snd (List.find (fun (id',_) -> id'=id) env)) "ASTLvNth(lv id)" in

      if (i >= 0 && i<size) then 
        adr+i, mem'
      else
        failwith "index out of bounds"

    |ASTLvNth (lv, e) ->
      let a1, mem' = eval_lvalue env mem lv in
      let a2, _ = Helper.match_value_for_MemoryBlock mem.memory.(a1) "ASTLvNth(lv)" in
      let ve, mem'' = eval_expr env mem' e in
      let i = Helper.match_value_for_InZ ve "ASTLVNth(ve)" in
      a2+i, mem''

and eval_stat (env: environement) (mem: memory) (out: output): stat ->  memory * output = function

    ASTEcho e -> 
        let v, mem = eval_expr env mem e in
        let i = Helper.match_value_for_InZ v "ASTEcho" in
        mem, i :: out
      
    | ASTSet(lv, e) -> 
        let ve, mem' = eval_expr env mem e in
        let a, mem'' = eval_lvalue env mem' lv in 
        
        begin
          match ve with 
          | InZ n -> 
            mem''.memory.(a) <- Current n;
            mem'', out
          | InBlock b ->
            mem''.memory.(a) <- MemoryBlock b;
            mem'', out
          |_ -> failwith (Printf.sprintf "Set: expression should evaluate to InZ or InBlock")
        end          
        
    | ASTIfStat(e, bk1, bk2) ->    
      let v, mem' = eval_expr env mem e in
      let cond_i = Helper.match_value_for_InZ v "ASTIfStat" in

      if cond_i = 1 then 
        eval_block env mem' out bk1
      else 
        eval_block env mem' out bk2
  
    | ASTWhile(e, bk) -> 
      let v, mem' = eval_expr env mem e in
      let cond_i = Helper.match_value_for_InZ v "While" in

      if cond_i = 1 then 
        let mem'', out' = eval_block env mem' out bk in
        (* effet de bord est attendu sur e pour que la boucle termine*)
        eval_stat env mem'' out' (ASTWhile(e, bk))
      else mem', out

    | ASTCall(id, es) -> 
      (*APP et APPR*)
      let vp = snd (List.find (fun (id', _) -> id = id') env) in
      let new_mem, vs = Helper.eval_es (eval_exprP env) mem es in
      begin match vp with
        | InP (bk, params, env') ->
            eval_block (Helper.bind env' params vs) new_mem out bk

        | InPR (bk, p_name, params, env') as self ->
            eval_block ((p_name, self) :: Helper.bind env' params vs) new_mem out bk

        | _ ->
            failwith "app on a non fonctionnel value"
      end

(* eval_exprP : Pour gérer le passage d'argument avec 'adr'*)
and eval_exprP (env: environement) (mem: memory): exprP -> value * memory = function
  | ASTexpr e -> eval_expr env mem e
  | ASTAdr id -> Helper.match_value_for_InA_v (snd (List.find (fun (id', _) -> id = id') env)), mem

and eval_def (env: environement) (mem: memory): def -> environement * memory = function

    ASTConst (id, _, e) ->
        let v, mem = eval_expr env mem e in
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
      (* On a pas explicitement un 'var xi' le var est inclus dans le constructeur ASTArgP *)
      (id, InP(bk, List.map (function ASTArg'(ASTArg (ident, _)) | ASTArgP (ident, _) -> ident ) args, env))
      ::env,
      mem

    |ASTProcREC(id, args, bk) ->
      (id, InPR(bk, id, List.map (function ASTArg'(ASTArg (ident, _)) | ASTArgP (ident, _) -> ident ) args, env))
      ::env,
      mem

and eval_expr (env: environement) (mem: memory): expr -> value * memory = function

    | ASTNum n ->
      (*Note: pour la section 'Fonctions sémantiques utiles' des notes de cours APS0.
      Ici la conversion est faite par le lexer (int_of_string)*)
        InZ n, mem
    
    | ASTId x ->
        let _, v = List.find (fun (id, _) -> id = x) env in
        begin
          match v with 
            | InA a -> 
              begin
                match mem.memory.(a) with 
                  | Current n -> InZ n, mem
                  | MemoryBlock b -> InBlock b, mem
                  | Any -> failwith "Acces of non initialized memory"
              end
            | _ -> v, mem
        end
        
    | ASTIf (e1, e2, e3) ->
      let v, mem = eval_expr env mem e1 in
      let cond_i = Helper.match_value_for_InZ v "functionnal if" in

      if cond_i = 1 then 
        eval_expr env mem e2
      else 
        eval_expr env mem e3
  
    | ASTAnd (e1, e2) ->
      let v1, mem' = eval_expr env mem e1 in
      let i1 = Helper.match_value_for_InZ v1 "And (e1)" in

      if i1 = 1 then
        let v2, mem'' = eval_expr env mem' e2 in
        let i2 = Helper.match_value_for_InZ v2 "And (e2)" in
        InZ i2, mem''
      else
        InZ i1, mem'

    | ASTOr (e1, e2) ->
      let v1, mem' = eval_expr env mem e1 in
      let i1 = Helper.match_value_for_InZ v1 "Or (e1)" in

      if i1 = 1 then
        InZ i1, mem'
      else
        let v2, mem'' = eval_expr env mem e2 in
        let i2 = Helper.match_value_for_InZ v2 "Or (e2)" in
        InZ i2, mem''

    | ASTApp (ASTId f, es) when StringMap.mem f pi1 || StringMap.mem f pi2 ->
      (*PRIM1 et PRIM2*)
      let new_mem, vs = Helper.eval_es (eval_expr env) mem es in

      begin match f, vs with
        | "not", [InZ n] ->
            InZ ((StringMap.find f pi1) n), new_mem
        | "div", [InZ n1; InZ n2] ->
          if (n2 = 0) 
            then failwith "Division by 0"
          else
            InZ ((StringMap.find f pi2) n1 n2), new_mem
        | ("eq" | "lt" | "add" | "sub" | "mul" ), [InZ n1; InZ n2] ->
            InZ ((StringMap.find f pi2) n1 n2), new_mem
        | _ ->
            let l = List.map (fun v -> Printf.sprintf "%s" (Helper.string_of_value v) ) vs in
            failwith (Printf.sprintf "primitive : %s applied erroneously on : %s" f (String.concat " " l))
      end

    | ASTApp (e, es) ->
      (*APP et APPR*)
      let vf, mem' = eval_expr env mem e in
      let new_mem, vs = Helper.eval_es (eval_expr env) mem' es in

      begin match vf with
      | InF (e_body, params, env') ->
          eval_expr (Helper.bind env' params vs) new_mem e_body

      | InFR (e_body, f_name, params, env') as self ->
          eval_expr ((f_name, self) :: Helper.bind env' params vs) new_mem e_body

      | _ ->
          failwith "app on a non fonctionnel value"
      end
      

    | ASTLambda (args, e_body) ->
        InF(e_body, List.map (function ASTArg (ident, _) -> ident) args, env), mem 
        
    | ASTAlloc e ->
      let v, mem' = eval_expr env mem e in
      let n = Helper.match_value_for_InZ v "ASTalloc" in
      let adr, mem'' = allocn mem' n in
      InBlock {adr; size=n}, mem''

    | ASTNth (e1, e2) ->
      let v1, mem' = eval_expr env mem e1 in
      let adr, size = Helper.match_value_for_InBlock v1 "ASTNth(e1)" in

      let v2, mem'' = eval_expr env mem' e2 in
      let i = Helper.match_value_for_InZ v2 "ASTNth(e2)" in

      if(i >= 0 && i < size) then 
        begin 
          match mem.memory.(adr+i) with
            | Current n -> InZ n
            | MemoryBlock memory_block -> InBlock memory_block
            | Any -> failwith"ASTNth: non init memory"  
        end
        , mem''
      else
        (*NOTE: non spécifié*)
        failwith "index out of bounds"

    | ASTLen e ->
      let v, mem' = eval_expr env mem e in
      let _, size = Helper.match_value_for_InBlock v "ASTLen" in
      InZ size, mem'

    |ASTVset (e1, e2, e3) ->
      let v1, mem' = eval_expr env mem e1 in
      let a, size = Helper.match_value_for_InBlock v1 "ASTVset(e1)" in

      let v2, mem'' = eval_expr env mem' e2 in
      let i = Helper.match_value_for_InZ v2 "ASTVset(e2)" in

      let v3, mem''' = eval_expr env mem'' e3 in
      let memory_v3 = Helper.match_value_for_mem_value v3 "ASTVset(e3)" in
      
      mem'''.memory.(a+i) <- memory_v3;
      (InBlock {adr = a; size}, mem''')

      




