open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm
open Aps_syntax.Interpreter

let l_test_0 = [
  (testfile_name 0 0, "OK", [11]);
  (testfile_name 0 1, "KO", []);
  (testfile_name 0 2, "KO", []);
  (testfile_name 0 3, "KO", []);
  (testfile_name 0 4, "OK", [1]);
  (testfile_name 0 5, "OK", [45]);
  (testfile_name 0 6, "OK", [25]);
  (testfile_name 0 7, "OK", [54])
]

let l_test_1 = [
  (testfile_name 1 0, "OK", [3]);
  (testfile_name 1 1, "OK", [7]);
  (testfile_name 1 2, "OK", [1]);
  (testfile_name 1 3, "OK", [9]);
  (testfile_name 1 4, "OK", [2]);
  (testfile_name 1 5, "OK", [5;8]);
  (testfile_name 1 6, "OK", [1]);
  (testfile_name 1 7, "OK", [1;2;3;4;5]);
  (testfile_name 1 8, "OK", [42]);
  (testfile_name 1 9, "OK", [52]);
  (*1a*)
  (testfile_name 1 10, "KO", []); (* Set sur Const*)
  (testfile_name 1 11, "OK", [5]);
  (testfile_name 1 12, "KO", []); (* Constante mais déclarée comme var dans la signature*)
  (testfile_name 1 13, "KO", []); (* Manque le var dans la signature*)
  (* TODO: ici ya pas d'erreur parceque VAR x int introduit déja x avec ref(int) donc l'absence de (adr x) ne pose pas de pb*)
  (testfile_name 1 14, "KO", []); (* CALL sans adr pour l'argument*)
]

let l_test_2 = [
  (testfile_name 2 0, "OK", [5]);
  (testfile_name 2 1, "OK", [10; 20]);
  (testfile_name 2 2, "OK", [42; 84]);
  (testfile_name 2 3, "OK", [10;11;12;13;14]);
]

(* Map entre la version d'aps et sa suite de tests*)
module IMap = Map.Make(Int)
let version_test_suit = IMap.of_list 
  [
    (0, l_test_0);
    (1, l_test_1);
    (2, l_test_2);
  ]

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
    Format.printf "\n********* Tests de APS %d ***********  \n" aps_version;
    test_pipeline (IMap.find aps_version version_test_suit)

let _ =
  (* None pour lancer les tests de toutes les versions *)
  let test_number = Some 2 in

  match test_number with
    | Some i -> lunch i;
    (* Si rien n'est précisé on lance tout*)
    | None ->
      IMap.iter (fun version _ -> lunch version ) version_test_suit
