(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
  [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  (* Symboles *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'              { SEMCOL }
  | ':'              { COL }
  | ','              { COMMA }
  | '*'              { STAR }
  | "->"             { ARROW }
  (* Mots clés *)
  | "ECHO"           { ECHO }
  | "CONST"          { CONST }
  | "FUN"            { FUN }  
  | "REC"            { REC }
  | "ECHO"           { ECHO }
  | "bool"           { BOOL }
  | "int"            { INT }
  | "if"             { IF }
  | "and"            { AND }
  | "or"             { OR }
  (* APS1 *)
  | "VAR"            { VAR }
  | "PROC"           { PROC }
  | "SET"            { SET }
  | "IF"             { IF_STAT }
  | "WHILE"          { WHILE }
  | "CALL"           { CALL }
  | "void"           { VOID }

  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | _ as c {
    let p = Lexing.lexeme_start_p lexbuf in
    failwith (Printf.sprintf
      "Unexpected char '%c' at line %d, col %d"
      c p.pos_lnum (p.pos_cnum - p.pos_bol + 1))
  }
