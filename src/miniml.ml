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
    Miniml_parser.prog Miniml_lexer.read lexbuf |> check_type_prog
  end with
  | SyntaxError msg ->
    printf "%a: %s\n" print_position lexbuf msg;
    Error(UnboundVar("lol"))

  | Miniml_parser.Error ->
    printf "%a: syntax error\n" print_position lexbuf;
    Error(UnboundVar("lol"))

let print_sexp sexp =  Sexp.to_string_hum sexp |> printf "%s\n"

let rec parse_and_print lexbuf =
  let print_constraints (c1, c2) = 
    let s1 = Ast.sexp_of_tyname c1 |> Sexp.to_string_hum in
    let s2 = Ast.sexp_of_tyname c2 |> Sexp.to_string_hum in
    printf "(%s, %s)\n\n" s1 s2
  in
  let print_env env =
    let print_binding (name, (_,ty)) =
      printf "%s: " name;
      Ast.sexp_of_tyname ty |> print_sexp;
    in
    printf "Type synonyms:\n";
    List.iter (Map.to_alist env.tysyms) print_binding;
    printf "Bindings:\n";
    List.iter (Map.to_alist env.bindings) print_binding
  in
  match parse_with_error lexbuf with
  | Ok(env) -> print_env env
  | Error(tyerr) -> Typecheck.sexp_of_tyerror tyerr |> print_sexp
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
