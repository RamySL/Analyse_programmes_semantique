open Aps_syntax.Manip_sys
open Aps_syntax.PrologTerm
open Aps_syntax.Interpreter
let l_test_0 = [
  (testfile_name 0 0, "OK");
  (testfile_name 0 1, "KO");
  (testfile_name 0 2, "KO");
  (testfile_name 0 3, "KO");
  (testfile_name 0 4, "OK");
  (testfile_name 0 5, "OK");
  (testfile_name 0 6, "OK");
  (testfile_name 0 7, "OK")
]

let l_test_1 = [
  (testfile_name 1 0, "OK");
  (testfile_name 1 1, "OK");
  (testfile_name 1 2, "OK");
  (testfile_name 1 3, "OK");
  (testfile_name 1 4, "OK");
  (testfile_name 1 5, "OK");
  (testfile_name 1 6, "OK");
  (testfile_name 1 7, "OK");
  (testfile_name 1 8, "OK");
  (testfile_name 1 9, "OK");
]

(** Affiche pour chaque fichier la représentation Prolog du programme parsé *)
let test_prologTerm (l_test : string list) =
  List.iter
    (fun fname ->
      let p = get_prog fname in
      Format.printf "%s |\t %a\n" fname pp_prog p
    )
    l_test


(** Exécute le pipeline complet sur un fichier :
    1) parsing
    2) génération du terme Prolog
    3) appel du typeur
    4) si le typage réussit, exécution de l'interprète
    5) sinon, on n'exécute pas le programme *)
let run_one_file (fname : string) (expected : string) =
  let p = get_prog fname in
  pp_prog Format.str_formatter p;
  let s = Format.flush_str_formatter () in

  Format.printf "\n===== %s =====\n" fname;

  match cmd_typ s with
  | Ok (res, _) ->
      let res = String.trim res in
      Format.printf "Typeur      : %s\n" res;
      Format.printf "Attendu     : %s\n" expected;

      if res = "OK" then begin
        Format.printf "Execution   : OK\n";
        List.iter (fun i -> Format.printf "%d\n" i) (snd(eval_prog p))
      end else begin
        Format.printf "Execution   : non lancee (erreur de type)\n"
      end

  | Error (`Msg m) ->
      Format.printf "Erreur typeur/systeme : %s\n" m;
      Format.printf "Execution   : non lancee\n"

let test_pipeline (l_test : (string * string) list) =
  List.iter (fun (fname, expected) -> run_one_file fname expected) l_test

(* TODO: mettre en param le choix de quelle suite de test lancer*)
let _ =
  
  Format.printf "========== Tests de APS 0 ==========\n";
  Format.printf "- Test de PrologTerm\n";
  test_prologTerm (List.map fst l_test_0);
  Format.printf "\n- Pipeline complet : typage puis execution\n";
  test_pipeline l_test_0;
  
  Format.printf "========== Tests de APS 1 ==========\n";
  Format.printf "- Test de PrologTerm\n";
  test_prologTerm (List.map fst l_test_1);
  Format.printf "\n- Pipeline complet : typage puis execution\n";
  test_pipeline l_test_1