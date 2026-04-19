open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm
open Aps_syntax.Interpreter
open Aps_syntax.Tests_helper

(** Exécute le pipeline complet sur un fichier :
    1) parsing
    2) génération du terme Prolog
    3) appel du typeur
    4) si le typage réussit, exécution de l'interprète et comparaison avec l'évaluation attendue
       - Some l : on compare la sortie obtenue à l
       - None   : on vérifie qu'une exception Failure est levée
    5) sinon, on n'exécute pas le programme *)
let run_one_file (fname : string) (expected_typ : string) (expected_eval : int list option) =
  let p = get_prog fname in
  pp_prog Format.str_formatter p;
  let s = Format.flush_str_formatter () in

  match cmd_typ s with
  | Ok (res, _) ->
      let res_trim = String.trim res in
      let typing_ok = (res_trim = expected_typ) in
      let typing_emoji = if typing_ok then "✅" else "❌" in

      Format.printf "\n===== %s =====\n" fname;
      Format.printf "Typage  : %s\n" typing_emoji;
      Format.printf "Typeur  : %s\n" res_trim;
      Format.printf "Attendu : %s\n" expected_typ;

      if res_trim = "OK" then begin
        match expected_eval with
        | Some expected ->
            begin
              try
                let actual_eval = snd (eval_prog p) in
                let eval_ok = (actual_eval = expected) in
                let eval_emoji = if eval_ok then "✅" else "❌" in
                Format.printf "\nEvaluation : %s\n" eval_emoji;
                Format.printf "Evaluation attendue : %s\n" (string_of_int_list expected);
                Format.printf "Evaluation obtenue  : %s\n" (string_of_int_list actual_eval)
              with
              | Failure msg ->
                  Format.printf "\nEvaluation : ❌\n";
                  Format.printf "Evaluation attendue : %s\n" (string_of_int_list expected);
                  Format.printf "Erreur obtenue      : Failure(%s)\n" msg
            end
        | None ->
            begin
              try
                let actual_eval = snd (eval_prog p) in
                Format.printf "\nEvaluation : ❌\n";
                Format.printf "Evaluation attendue : %s\n" (string_of_expected_eval None);
                Format.printf "Evaluation obtenue  : %s\n" (string_of_int_list actual_eval)
              with
              | Failure msg ->
                  Format.printf "\nEvaluation : ✅\n";
                  Format.printf "Evaluation attendue : %s\n" (string_of_expected_eval None);
                  Format.printf "Erreur obtenue      : Failure(%s)\n" msg
            end
      end else begin
        Format.printf "Evaluation : non lancee (typage KO)\n"
      end

  | Error (`Msg m) ->
      Format.printf "\n===== %s =====\n" fname;
      Format.printf "Erreur typeur/systeme : %s\n" m;
      Format.printf "Evaluation : non lancee\n"

(* Lance le pipeline sur une liste de fichiers *)
let test_pipeline (l_test : (string * string * int list option) list) =
  List.iter (fun (fname, expected_typ, expected_eval) ->
    run_one_file fname expected_typ expected_eval
  ) l_test

(** Lance la suite de tests correspondante a aps_version *)
let lunch aps_version =
  Format.printf "\n********* Tests de APS %s ***********\n" aps_version;
  test_pipeline (SMap.find aps_version version_test_suit)

let _ =
  let argc = Array.length Sys.argv in
  let prog_name = Sys.argv.(0) in
  match argc with
  | 1 ->
      SMap.iter (fun version _ -> lunch version) version_test_suit
  | 2 ->
      let version = Sys.argv.(1) in
      if SMap.mem version version_test_suit then
        lunch version
      else begin
        Format.eprintf
          "Version inconnue : %s\nVersions possibles : %s\n"
          version
          (available_versions_string ());
      end
  | _ ->
      usage prog_name;
