(* Dans ce fichier sont déf les types de données nécessaire pour faire la sémantique du programme

- On doit définir les types de:
  - Environement d'evaluation E: ident -> V (ident = id ou string ?)
    - L'ensemble des valeurs APS V: inZ(n), inF(f), inFR(fr) où n ∈ Z, f ∈ F, fr ∈ FR
      - Une fermeture de fonction non rec. F: expr * (ident*\) * E
      - Une fermeture de fonction rec. FR: expr * (ident) * (ident*\) * E

  - le flux de sortie O: c'est juste Z* uen suite de nombre 
*)

open Ast

(* liste chainés optimal pour un environement qui doit fonctionner comme une LIFO pour suivre les scops*)
type environement = (string * value) list

and value = InZ of int | InF of closure | InFR of rec_closure

and closure = expr * string list * environement
                    (*nom de la fct, nom de ces params*)
and rec_closure =  expr * string * string list * environement

and output = int list