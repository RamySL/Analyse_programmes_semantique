open Types

(*
  Retourne le string représentant l'environement
*)
let rec string_of_environement (e: environement) = 
  
  let l = List.map (fun (id, v) -> Printf.sprintf "(%s, %s)" id (string_of_value v) ) e in
  String.concat "\n" l


and string_of_value (v: value) = 
  match v with
    | InZ n -> Printf.sprintf "InZ(%d)" n 
    | InF c -> Printf.sprintf "InF(%s)" (string_of_closure c)
    |InFR rc ->Printf.sprintf "InFR(%s)" (string_of_rec_closure rc)

and string_of_closure (c:closure) = 
  let (_, args, env) = c in
  (*TODO: un string_of_expr pour le body*)
  Printf.sprintf "body, [%s], %s" (String.concat " " args) (string_of_environement env)


and string_of_rec_closure (rc:rec_closure) = 
  let (_, f, args, env) = rc in
  (*TODO: un string_of_expr pour le body*)
  Printf.sprintf "body, %s,  [%s], %s" f (String.concat " " args) (string_of_environement env)

and string_of_output (out:output) =
  String.concat " " (List.map (fun o -> string_of_int o) out)


let print_environement (env: environement) = 
    Printf.printf "Environement : \n%s \n" (string_of_environement env)


(**Retourne l'environement env etendu avec tous les binding formé par les éléments
    de params et les valeurs de vs*)
let bind (env:environement) params vs : environement =
      if List.length params <> List.length vs then
        failwith "arity mismatch"
      else
        List.fold_left2 (fun acc p v -> (p, v) :: acc) env params vs

