open Bos


let testfile_name ver i = Printf.sprintf "examples/APS%d/prog%d.aps" ver i

let typ_path = "lib/Typer/typing_rules.pl"

let cmd_typ pl_term =
  OS.Cmd.(
    in_string pl_term |>
    run_io (Cmd.(v "swipl" % "-g" % "main" % "-t" %"halt" % typ_path)) |>
    out_string)

let get_prog (fname : string) : Ast.prog =
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  try
    let p = Parser.prog Lexer.token lexbuf in
    close_in ic;
    p
  with
  | Lexer.SyntaxError msg ->
      close_in_noerr ic;
      prerr_endline msg;
      exit 1

  | Parser.Error ->
      close_in_noerr ic;
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      let tok = Lexing.lexeme lexbuf in
      Printf.eprintf
        "%s:%d:%d: syntax error near %S\n"
        fname line col tok;
      exit 1

  | Lexer.Eof ->
      close_in_noerr ic;
      Printf.eprintf "%s: unexpected end of file\n" fname;
      exit 1