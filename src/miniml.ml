open Core.Std
open Miniml_lexer
open Lexing
open Ast
open Typecheck
open Interp

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try begin
    Miniml_parser.prog Miniml_lexer.read lexbuf
  end with
  | SyntaxError msg ->
    sprintf "%s: %s\n" (print_position lexbuf) msg |> failwith

  | Miniml_parser.Error ->
    print_position lexbuf |> sprintf "%s: syntax error\n" |> failwith
;;

let print_sexp sexp =  Sexp.to_string_hum sexp |> printf "%s\n"

let get_lexbuf action filename () = 
  try begin 
    In_channel.with_file filename ~f:(fun inx ->
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      protect ~f:(fun () -> action lexbuf) ~finally:(fun () -> In_channel.close inx))
  end with
  | Failure(msg) -> printf "%s\n" msg
  | Sys_error(msg) -> printf "%s\n" msg
;;

let progdef_to_string p = p |> Ast.sexp_of_progdef |> Sexp.to_string_hum

let parse =
  let run_parse lexbuf =
    parse_with_error lexbuf
    |> List.iter ~f:(fun p -> p |> progdef_to_string |> printf "%s\n")
  in
  Command.basic ~summary:"parse a miniml program"
    Command.Spec.(empty +> anon ("filename" %: file))
    (get_lexbuf run_parse)
;;

let typecheck =
  let print_constraints (c1, c2) = 
    let s1 = Ast.sexp_of_tyname c1 |> Sexp.to_string_hum in
    let s2 = Ast.sexp_of_tyname c2 |> Sexp.to_string_hum in
    printf "(%s, %s)\n\n" s1 s2
  in
  let print_binding (name, (_,ty)) =
    printf "%s: " name;
    Ast.sexp_of_tyname ty |> print_sexp;
  in
  let print_sum (name, (tyname, args, def)) =
    let arglist = List.fold args ~init:"" ~f:(fun acc x -> acc ^ " " ^ x) in
    match def with
    | None ->
      printf "%s -> %s %s\n" name arglist tyname
    | Some(tydef) ->
      let tydefstr = tydef |> Ast.sexp_of_tyname |> Sexp.to_string_hum in
      printf "%s -> %s %s = %s\n" name arglist tyname tydefstr
  in
  let print_env env =
    printf "Type synonyms:\n";
    List.iter (Map.to_alist env.tysyms) print_binding;
    printf "\nConstructors:\n";
    List.iter (Map.to_alist env.tysums) print_sum;
    printf "\nBindings:\n";
    List.iter (Map.to_alist env.bindings) print_binding;
  in
  let run_typecheck e c lexbuf =
    match parse_with_error lexbuf |> check_type_prog with
    | Ok(tyinfo) ->
      if e then begin
        tyinfo.info |> Typecheck.string_of_subst |> printf "subst: %s\n"
      end;
      if c then begin
        printf "\nconstraints:\n";
        List.iter tyinfo.constraints print_constraints;
      end;
      print_env tyinfo.env;
      printf "\nSUCCESS!\n";

    | Error(errinfo) ->
      if e then print_env errinfo.env else ();
      if c
      then begin
        printf "\nconstraints:\n";
        List.iter errinfo.constraints print_constraints;
      end
      else ();
      Typecheck.sexp_of_tyerror errinfo.info |> print_sexp;
      printf "\nERROR!\n"
  in
  Command.basic ~summary:"typecheck a miniml program"
    Command.Spec.(
      empty
      +> flag "-e" no_arg ~doc:" print environment"
      +> flag "-c" no_arg ~doc:" print constraints"
      +> anon ("filename" %: file))
    (fun e c -> get_lexbuf (run_typecheck e c))
;;

let interp = 
  let run_interp lexbuf =
    let open Result in
    let ast = parse_with_error lexbuf in
    match check_type_prog ast with
    | Ok(_) ->
      begin match interp_prog ast with
      | Ok(valmap) ->
        let main_val = Map.find_exn valmap "main" in
        printf "%s\n" (Util.sprint_interp_val main_val)

      | Error(interp_err) ->
        interp_err |> Util.sprint_interp_err |> printf "%s\n"
      end
    | Error(tyerror) -> printf "typechecking error!"
  in
  Command.basic ~summary:"interpret a miniml program"
    Command.Spec.(
      empty
      +> anon ("filename" %: file)
    )
    (get_lexbuf run_interp)
;;

let () =
  let command =
    Command.group ~summary:"compiler for miniml"
      [ "parse", parse; "typecheck", typecheck; "interp", interp]
  in
  Command.run command
;;

