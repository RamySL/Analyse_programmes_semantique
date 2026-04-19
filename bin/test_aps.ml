open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm
open Aps_syntax.Interpreter
open Aps_syntax.Tests_helper

(** Exécute le pipeline complet sur un fichier :
    1) parsing
    2) génération du terme Prolog
    3) appel du typeur
    4) si le typage réussit, exécution de l'interprète et comparaison avec l'évaluation attendue
    5) sinon, on n'exécute pas le programme *)
let run_one_file (fname : string) (expected_typ : string) (expected_eval : int list) =
  let p = get_prog fname in
  pp_prog Format.str_formatter p;
  let s = Format.flush_str_formatter () in

  match cmd_typ s with
  | Ok (res, _) ->
      let res_trim = String.trim res in
      let typing_imoji = if (res_trim = expected_typ) then "✅" else "❌" in

      Format.printf "\n===== %s =====\n" fname;
      Format.printf "Typage  : %s\n" typing_imoji;
      Format.printf "Typeur  : %s\n" res_trim;
      Format.printf "Attendu : %s\n" expected_typ;

      if res_trim = "OK" then begin
        let actual_eval = snd(eval_prog p) in
        
        let str_expected_eval = "[" ^ String.concat "; " (List.map string_of_int expected_eval) ^ "]" in
        let str_actual_eval = "[" ^ String.concat "; " (List.map string_of_int actual_eval) ^ "]" in
        
        let eval_imoji = if actual_eval = expected_eval then "✅" else "❌" in
        Format.printf "\nEvaluation : %s\n" eval_imoji;
        Format.printf "Evaluation attendu: %s\n" str_expected_eval;
        Format.printf "Evaluation obtenu : %s\n" str_actual_eval;

      end else begin
        Format.printf "Evaluation : non lancee (erreur de type ou programme KO attendu)\n"
      end

  | Error (`Msg m) ->
      Format.printf "Erreur typeur/systeme : %s\n" m;
      Format.printf "Evaluation : non lancee\n"

(* Lance le pipeline sur une liste de fichiers*)
let test_pipeline (l_test : (string * string * int list) list) =
  List.iter (fun (fname, expected_typ, expected_eval) -> 
    run_one_file fname expected_typ expected_eval
  ) l_test


(** Lance la suite de tests correspendante à aps-version*)
let lunch aps_version = 
    Format.printf "\n********* Tests de APS %s ***********  \n" aps_version;
    test_pipeline (SMap.find aps_version version_test_suit)

let _ =
  (* None pour lancer les tests de toutes les versions *)
  let test_number = Some "2" in

  match test_number with
    | Some i -> lunch i;
    (* Si rien n'est précisé on lance tout*)
    | None ->
      SMap.iter (fun version _ -> lunch version ) version_test_suit
