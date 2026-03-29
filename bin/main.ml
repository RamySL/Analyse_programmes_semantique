open Aps_syntax
open PrologTerm
open Manip_sys
open Interpreter


let print_prog () =

  let fname = Sys.argv.(1) in
  let p = get_prog fname in

    pp_prog Format.str_formatter p;
    let s = Format.flush_str_formatter () in
    Format.printf "==== Test du pretty printer de termes ====\n %s " s ;
    Format.printf "==== Test du typage du programme ====\n" ;
    
    match cmd_typ s with
    | Ok (res, _) ->
        let res = String.trim res in
        Format.printf "%s\n" res;
        if res = "OK" then begin
          Format.printf "==== Evaluation du programme donne comme sortie ====\n";
          List.iter (fun i -> Format.printf "%d\n" i) (eval_prog p)
        end else begin
          prerr_endline "Erreur de type";
          exit 1
        end
    | Error (`Msg m) ->
        prerr_endline m;
        exit 1
    


let _ = print_prog ()