(* Liste de tests: nom de fichier, resultat de typage attendu, et resultat d'evaluation attendu *)

let l_test_0 = [
  (Manip_sys.testfile_name "0" 0, "OK", Some [6]); (* lambda *)
  (Manip_sys.testfile_name "0" 1, "KO", None); (* variable non déclarée*)
  (Manip_sys.testfile_name "0" 2, "KO", None); (* variable non déclarée*)
  (Manip_sys.testfile_name "0" 3, "OK", Some [12]); (* fonction qui retourne une fonction *)
  (Manip_sys.testfile_name "0" 4, "OK", Some [1]); (* PRIM *)
  (Manip_sys.testfile_name "0" 5, "OK", Some [14]); (* Portée statique *)
  (Manip_sys.testfile_name "0" 6, "OK", Some [25]); (* fonction simple *)
  (Manip_sys.testfile_name "0" 7, "OK", Some [120]); (* factoriel *)
  (Manip_sys.testfile_name "0" 8,  "OK", Some [7]); (* if *)
  (Manip_sys.testfile_name "0" 9,  "OK", Some [2]); (* and*)
  (Manip_sys.testfile_name "0" 10, "OK", Some [1]); (* or *)
  (Manip_sys.testfile_name "0" 11, "OK", None); (*division par zéro*)

  (Manip_sys.testfile_name "0" 12, "KO", None); (* init avec mauvais type*)
  (Manip_sys.testfile_name "0" 13, "KO", None); (* mauvais type de retour *)
  (Manip_sys.testfile_name "0" 14, "KO", None); (* mauvais argument *)
  (Manip_sys.testfile_name "0" 15, "KO", None); (* manque un argument *)
  (Manip_sys.testfile_name "0" 16, "KO", None); (* type différent pour les deux branches*)
]

let l_test_1 = [
  (Manip_sys.testfile_name "1" 0, "OK", Some [3]); 
  (Manip_sys.testfile_name "1" 1, "OK", Some [7]);
  (Manip_sys.testfile_name "1" 2, "OK", Some [1]);
  (Manip_sys.testfile_name "1" 3, "OK", Some [9]);
  (Manip_sys.testfile_name "1" 4, "OK", Some [2]);
  (Manip_sys.testfile_name "1" 5, "OK", Some [5; 8]);
  (Manip_sys.testfile_name "1" 6, "OK", Some [1]);
  (Manip_sys.testfile_name "1" 7, "OK", Some [1; 2; 3; 4; 5]);
  (Manip_sys.testfile_name "1" 8, "OK", Some [42]);
  (Manip_sys.testfile_name "1" 9, "OK", Some [52]);
  (Manip_sys.testfile_name "1" 10, "OK", Some [5; 5]); (* procédure d'ordre sup *)
  (Manip_sys.testfile_name "1" 11, "KO", None); (* Set sur Const *)
]

let l_test_1a = [
  (Manip_sys.testfile_name "1a" 1, "OK", Some [5]); (* 'var' et 'adr' utilisé sur une variable comme ça doit se faire*)
  (Manip_sys.testfile_name "1a" 2, "KO", None); (* Constante mais declarée comme var dans la signature *)
  (Manip_sys.testfile_name "1a" 3, "KO", None); (* Manque le var dans la signature *)
  (* TODO: ici ya pas d'erreur parceque VAR x int introduit deja x avec ref(int) donc l'absence de (adr x) ne pose pas de pb *)
  (Manip_sys.testfile_name "1a" 4, "KO", None); (* CALL sans adr pour l'argument *)
]

let l_test_2 = [
  (Manip_sys.testfile_name "2" 0, "OK", Some [5]); (* le n *)
  (Manip_sys.testfile_name "2" 1, "OK", Some [10; 20]); (* vset *)
  (Manip_sys.testfile_name "2" 2, "OK", Some [42; 84]); (* set *)
  (Manip_sys.testfile_name "2" 3, "OK", Some [10; 11; 12; 13; 14]);
  (Manip_sys.testfile_name "2" 4, "OK", Some [33]); (* matrice *)
  (Manip_sys.testfile_name "2" 5, "OK", Some [1]); (* tableau de bool *)
  (Manip_sys.testfile_name "2" 6, "OK", Some [99]); (* mutation de tableau par procedure *)

  (Manip_sys.testfile_name "2" 7,  "KO", None); (* alloc sur bool *)
  (Manip_sys.testfile_name "2" 8,  "KO", None); (* len sur int *)
  (Manip_sys.testfile_name "2" 9,  "KO", None); (* nth sur non tableau *)
  (Manip_sys.testfile_name "2" 10, "KO", None); (* vset de mauvais type *)

  (Manip_sys.testfile_name "2" 11, "OK", None); (* nth hors bornes *)
  (Manip_sys.testfile_name "2" 12, "OK", None); (* SET hors bornes *)
  (Manip_sys.testfile_name "2" 13, "OK", None); (* indice negatif *)
  (Manip_sys.testfile_name "2" 14, "OK", None); (* acces a un tableau vide *)

  (Manip_sys.testfile_name "2" 15, "KO", None); (* tableau de fonctions *)

  (Manip_sys.testfile_name "2" 16, "OK", Some [6]); (* test d'allocn dans interpreter.ml*)
]

module SMap = Map.Make(String)
let version_test_suit = SMap.of_list
  [
    ("0", l_test_0);
    ("1", l_test_1);
    ("1a", l_test_1a);
    ("2", l_test_2);
  ]

let string_of_int_list l =
  "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

let string_of_expected_eval = function
  | Some l -> string_of_int_list l
  | None -> "<erreur d'evaluation attendue>"

(* Versions d'aps*)
let available_versions () =
  List.map fst (SMap.bindings version_test_suit)

let available_versions_string () =
  String.concat ", " (available_versions ())

let usage prog_name =
  Format.eprintf
    "Usage: %s [version]\nAucune version : lance tous les tests.\nVersions possibles : %s\n"
    prog_name
    (available_versions_string ())