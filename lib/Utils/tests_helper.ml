(* Liste de tests: nom de fichier, resultat de typage attendu, et resultat d'evaluation attendu*)

let l_test_0 = [
  (Manip_sys.testfile_name "0" 0, "OK", [11]);
  (Manip_sys.testfile_name "0" 1, "KO", []);
  (Manip_sys.testfile_name "0" 2, "KO", []);
  (Manip_sys.testfile_name "0" 3, "KO", []);
  (Manip_sys.testfile_name "0" 4, "OK", [1]);
  (Manip_sys.testfile_name "0" 5, "OK", [45]);
  (Manip_sys.testfile_name "0" 6, "OK", [25]);
  (Manip_sys.testfile_name "0" 7, "OK", [54])
]

let l_test_1 = [
  (Manip_sys.testfile_name "1" 0, "OK", [3]);
  (Manip_sys.testfile_name "1" 1, "OK", [7]);
  (Manip_sys.testfile_name "1" 2, "OK", [1]);
  (Manip_sys.testfile_name "1" 3, "OK", [9]);
  (Manip_sys.testfile_name "1" 4, "OK", [2]);
  (Manip_sys.testfile_name "1" 5, "OK", [5;8]);
  (Manip_sys.testfile_name "1" 6, "OK", [1]);
  (Manip_sys.testfile_name "1" 7, "OK", [1;2;3;4;5]);
  (Manip_sys.testfile_name "1" 8, "OK", [42]);
  (Manip_sys.testfile_name "1" 9, "OK", [52]);
]

let l_test_1a = [  (*1a*)
  (Manip_sys.testfile_name "1a" 0, "KO", []); (* Set sur Const*)
  (Manip_sys.testfile_name "1a" 1, "OK", [5]);
  (Manip_sys.testfile_name "1a" 2, "KO", []); (* Constante mais déclarée comme var dans la signature*)
  (Manip_sys.testfile_name "1a" 3, "KO", []); (* Manque le var dans la signature*)
  (* TODO: ici ya pas d'erreur parceque VAR x int introduit déja x avec ref(int) donc l'absence de (adr x) ne pose pas de pb*)
  (Manip_sys.testfile_name "1a" 4, "KO", []); (* CALL sans adr pour l'argument*)
  ]

let l_test_2 = [
  (Manip_sys.testfile_name "2" 0, "OK", [5]); (* le n*)
  (Manip_sys.testfile_name "2" 1, "OK", [10; 20]); (* vset *)
  (Manip_sys.testfile_name "2" 2, "OK", [42; 84]); (* set *)
  (Manip_sys.testfile_name "2" 3, "OK", [10;11;12;13;14]);
  (Manip_sys.testfile_name "2" 4, "OK", [33]); (* Matrice *)
  (Manip_sys.testfile_name "2" 5, "OK", [1]); (* tableau de bool *)
  (Manip_sys.testfile_name "2" 6, "OK", [99]); (* mutation de tableau par procédure *)
]

(* Map entre la version d'aps et sa suite de tests*)
module SMap = Map.Make(String)
let version_test_suit = SMap.of_list 
  [
    ("0", l_test_0);
    ("1", l_test_1);
    ("1a", l_test_1a);
    ("2", l_test_2);
  ]