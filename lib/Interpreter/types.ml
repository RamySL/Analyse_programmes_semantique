(* Dans ce fichier sont déf les types de données nécessaire pour faire la sémantique du programme *)

open Ast

(* liste chainés optimal pour un environement qui doit fonctionner comme une LIFO pour suivre les scops*)
type environement = (string * value) list

(** Valeurs APS : entiers, cloture de fonction, cloture de fonction recursive, Adresse mémoire, *)
and value = 
  | InZ of int 
  | InF of closure 
  | InFR of rec_closure 
  | InA of adress  
  | InP of procedure_closure 
  | InPR of rec_procedure_closure 
  | InBlock of memory_block

and closure = expr * string list * environement
                    (*nom de la fct, nom de ces params*)
and rec_closure =  expr * string * string list * environement

and procedure_closure = block * string list * environement

and rec_procedure_closure = block * string * string list * environement

and adress = int

and memory_block =  {adr : adress; size:int} 

(* Absence de valeur ou bien valeur courante*)
and memory_value = Any | Current of int | MemoryBlock of memory_block

(*and memory = adress ->  memory_value*) (* non réaliste avec l'implem*)

and output = int list
