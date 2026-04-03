(* Dans ce fichier sont déf les types de données nécessaire pour faire la sémantique du programme

- On doit définir les types de:
  - Environement d'evaluation E: ident -> V (ident = id ou string ?)
    - L'ensemble des valeurs APS V: inZ(n), inF(f), inFR(fr) où n ∈ Z, f ∈ F, fr ∈ FR
      - Une fermeture de fonction non rec. F: expr * (ident*\) * E
      - Une fermeture de fonction rec. FR: expr * (ident) * (ident*\) * E

  - le flux de sortie O: c'est juste Z* uen suite de nombre 

  -APS1:
    - avec la notion de mémoire on a 'Mem' notre mémoire, qui est une fonction de A -> int ou any ? 
    - alloc(σ) = (a, σ′) si et seulement si a 6 ∈ dom(σ) et σ′ = σ[a = any]
    - Adresse A :
      Mémoire S = A → Z (fonction partielle)
      Fermetures procédurales P = Cmds × ident∗ × E
      Fermetures procédurales récursives P R = Cmds × ident × ident∗ × E
      Valeurs V ⊕ = A ⊕ P ⊕ P R
*)

open Ast

(* liste chainés optimal pour un environement qui doit fonctionner comme une LIFO pour suivre les scops*)
type environement = (string * value) list

(** Valeurs APS : entiers, cloture de fonction, cloture de fonction recursive, Adresse mémoire, *)
and value = InZ of int | InF of closure | InFR of rec_closure | InA of adress  | InP of procedure_closure | InPR of rec_procedure_closure 

and closure = expr * string list * environement
                    (*nom de la fct, nom de ces params*)
and rec_closure =  expr * string * string list * environement

and procedure_closure = block * string list * environement

and rec_procedure_closure = block * string * string list * environement

and adress = int
(* Absence de valeur ou bien valeur courante*)
and memory_value = Any | Current of int

(*and memory = adress ->  memory_value*) (* non réaliste avec l'implem*)

and output = int list


module AdressOrd = struct
  type t = adress
  let compare = Int.compare
end
