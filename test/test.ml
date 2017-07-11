open Core.Std
open OUnit2

module Parser = Miniml_parser
module Lexer = Miniml_lexer

let parse_miniml str =
  let open Option in
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(str)" };
  try begin
      Miniml_parser.prog Miniml_lexer.read lexbuf
   |> List.hd_exn |> Ast.sexp_of_progdef |> return
  end with
  | _ -> None
;;

let test_parse fail_msg prog expected =
  let res =
    match parse_miniml prog with
    | Some(ast_prog) ->
      let ast_expected = expected |> Sexp.of_string in
      if Sexp.equal ast_prog ast_expected
      then true
      else begin
        printf "\nParsed:\n%s\n\nExpected:\n%s"
          (Sexp.to_string ast_prog) (Sexp.to_string ast_expected);
        false
      end
    | None -> false
  in
  assert_bool fail_msg res

let parse_test1 ctx =
  let prog = "
    type 'a list =
      | Cons of ('a * ('a list)) -> 'a -> 'a
      | Nil
    ;;"
  in
  let expected = "
  (Typedef
    (TySum list('a)
      ((ConDef Cons
        (TyFunc (TyProd ((TyVar 'a) (TyCon list((TyVar 'a)))))
                (TyFunc(TyVar 'a)(TyVar 'a))))
      (ConDefEmpty Nil))))"
  in
  test_parse "test 1 failed!" prog expected

let parse_test2 ctx =
  let prog = "
    let f (lst : 'a list) =
      match lst with
      | Cons(hd, _) -> hd + (f tl)
      | Nil -> 0
    ;;"
  in
  let expected = "
    (Expr
     (Let (Id f)
      (Func (IdWithType lst (TyCon list ((TyVar 'a))))
       (Match (Var lst)
        (((match_pat (PatCon Cons ((PatVar hd) (PatVar _))))
          (body (App (App (Var +) (Var hd)) (App (Var f) (Var tl)))))
         ((match_pat (PatCon Nil ())) (body (ILit 0))))))))"
  in
  test_parse "test 2 failed!" prog expected

let parse_test3 ctx =
  let prog = "
    let g x y z =
      if x
      then y
      else z
    ;;"
  in
  let expected = "
    (Expr
     (Let (Id g)
      (Func (Id x) (Func (Id y) (Func (Id z) (Cond (Var x) (Var y) (Var z)))))))"
  in
  test_parse "test 3 failed!" prog expected

let parse_test4 ctx = 
  let prog = "
    let h = (fun x -> { name=x; age=12 } >>= \"NICE\") ;;"
  in
  let expected = "
    (Expr
     (Let (Id h)
      (Func (Id x)
       (App
        (App (Var >>=)
         (Rec (((name name) (value (Var x))) ((name age) (value (ILit 12))))))
        (SLit NICE)))))"
  in
  test_parse "test 4 failed!" prog expected

let parse_test5 ctx =
  let prog = "
    let k x = x.name.nice ;;
  " in
  let expected = "
    (Expr (Let (Id k) (Func (Id x) (Field (Field (Var x) name) nice))))"
  in
  test_parse "test 5 failed!" prog expected

let parse_test6 ctx =
  let prog = "
    let m x = x.name.nice && true ;;"
  in
  let expected = "
    (Expr
     (Let (Id m)
      (Func (Id x)
       (App (App (Var &&) (Field (Field (Var x) name) nice)) (BLit true)))))"
  in
  test_parse "test 6 failed!" prog expected

let parse_test7 ctx =
  let prog = "
    let k = (1, 2, 3) ;;"
  in
  let expected = "
    (Expr (Let (Id k) (Tuple ((ILit 1) (ILit 2) (ILit 3)))))"
  in
  test_parse "test 7 failed!" prog expected

let suite =
  "parser" >::: [
    "parse_test1" >:: parse_test1;
    "parse_test2" >:: parse_test2;
    "parse_test3" >:: parse_test3;
    "parse_test4" >:: parse_test4;
    "parse_test5" >:: parse_test5;
    "parse_test6" >:: parse_test6;
    "parse_test7" >:: parse_test7;
  ]
;;

let () =
  run_test_tt_main suite
;;
