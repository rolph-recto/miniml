open Core.Std
open Miniml_lexer
open Lexing
open Ast
open Typecheck

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try begin
    let get_type p =
      match p with
      | Typedef(_) -> (p, Error(UnboundVar("lol")))
      | Expr(e) -> (p, check_type e)
    in
    Miniml_parser.prog Miniml_lexer.read lexbuf |> List.map ~f:get_type
  end with
  | SyntaxError msg ->
    printf "%a: %s\n" print_position lexbuf msg;
    []

  | Miniml_parser.Error ->
    printf "%a: syntax error\n" print_position lexbuf;
    []

let print_sexp sexp =  Sexp.to_string_hum sexp |> printf "%s\n\n"

let rec parse_and_print lexbuf =
  let print_ast_and_type (p, t) =
    Ast.sexp_of_progdef p |> print_sexp;
    match t with
    | Ok(ty) -> Ast.sexp_of_tyname ty |> print_sexp
    | Error(tyerr) -> Typecheck.sexp_of_tyerror tyerr |> print_sexp
  in 
  List.iter (parse_with_error lexbuf) ~f:print_ast_and_type
;;

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

(*
let rec loop () =
  let instr = input_line stdin in
  let lexbuf = Lexing.from_string instr in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };
  parse_and_print lexbuf
*)

(* part 2 *)
(*
let () =
  Command.basic ~summary:"Parse miniml files"
    Command.Spec.(empty)
    loop 
  |> Command.run
*)

let () =
  Command.basic ~summary:"Parse and display MiniML programs"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run
