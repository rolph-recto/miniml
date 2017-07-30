open Core.Std
open OUnit2

module Parser = Miniml_parser
module Lexer = Miniml_lexer

open Typecheck

let test_sexp fail_msg f test_in expected =
  let res =
    match f test_in with
    | Some(test_out) ->
      let sexp_expected = expected |> Sexp.of_string in
      if Sexp.equal test_out sexp_expected
      then true
      else begin
        printf "\nActual:\n%s\n\nExpected:\n%s"
          (Sexp.to_string_hum test_out) (Sexp.to_string_hum sexp_expected);
        false
      end
    | None -> false
  in
  assert_bool fail_msg res
;;

let parse_miniml str =
  let open Option in
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(str)" };
  try begin
      Miniml_parser.prog Miniml_lexer.read lexbuf |> return
  end with
  | _ -> None
;;

let parse_miniml_sexp str =
  let open Option in
  parse_miniml str >>= fun ast ->
  ast |> List.hd_exn |> Ast.sexp_of_progdef |> return
;;

let test_parse fail_msg prog expected =
  test_sexp fail_msg parse_miniml_sexp prog expected
;;

let parse_test1 ctx =
  let prog = "
    type list['a] =
      | Cons of ('a * (list['a])) -> 'a -> 'a
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
    let f (lst : list['a]) =
      match lst with
      | Cons(hd, _) -> hd + (f tl)
      | Nil -> 0
    ;;"
  in
  let expected = "
	(Binding (Id f)
	 (Func (IdWithType lst (TyCon list ((TyVar 'a))))                                           
		(Match (Var lst)                       
		 (((match_pat (PatCon Cons (PatTuple ((PatVar hd) PatWildcard))))
			 (body (App (App (Var +) (Var hd)) (App (Var f) (Var tl)))))
			((match_pat (PatConEmpty Nil)) (body (IntLit 0)))))))"
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
    (Binding (Id g)
      (Func (Id x) (Func (Id y) (Func (Id z) (Cond (Var x) (Var y) (Var z))))))"
  in
  test_parse "test 3 failed!" prog expected

let parse_test4 ctx = 
  let prog = "
    let h = (fun x -> { name=x; age=12 } >>= \"NICE\") ;;"
  in
  let expected = "
    (Binding (Id h)
      (Func (Id x)
       (App
        (App (Var >>=)
         (Rec (((name name) (value (Var x))) ((name age) (value (IntLit 12))))))
        (StrLit NICE))))"
  in
  test_parse "test 4 failed!" prog expected

let parse_test5 ctx =
  let prog = "
    let k x = x.name.nice ;;
  " in
  let expected = "
    (Binding (Id k) (Func (Id x) (Field (Field (Var x) name) nice)))"
  in
  test_parse "test 5 failed!" prog expected

let parse_test6 ctx =
  let prog = "
    let m x = x.name.nice && true ;;"
  in
  let expected = "
    (Binding (Id m)
      (Func (Id x)
       (App (App (Var &&) (Field (Field (Var x) name) nice)) (BoolLit true))))"
  in
  test_parse "test 6 failed!" prog expected

let parse_test7 ctx =
  let prog = "
    let k = (1, 2, 3) ;;"
  in
  let expected = "
    (Binding (Id k) (Tuple ((IntLit 1) (IntLit 2) (IntLit 3))))"
  in
  test_parse "test 7 failed!" prog expected

(* typechecker tests *)

let tycheck_miniml_binding str = 
  let open Option in
  parse_miniml str >>= fun ast ->
  match check_type_prog ast with
  | Ok(okinfo) ->
    Map.find_exn okinfo.env.bindings "test"
    |> snd |> Ast.sexp_of_tyname |> return
  | Error(errinfo) -> None
;;

let def_tycheck_context bind =
  let ctx = "
  type list['a] =
      | Cons of 'a * list['a]
      | Nil
  ;;"
  in
  ctx ^ bind
;;

let test_tycheck_binding ?(ctx=def_tycheck_context) fail_msg prog expected =
  test_sexp fail_msg tycheck_miniml_binding (ctx prog) expected
;;
let tycheck_binding_tests =
  let test1 =
    let prog = "
      let test = (1, 2, 3) ;;"
    in
    let expected  = "
      (TyProd ((TyCon int ()) (TyCon int ()) (TyCon int ())))"
    in 
    (prog, expected, "mono-tuple-tycheck")
  in
  let test2 = 
    let prog = "
      let rec test (n:int) = test (test n) ;;"
    in
    let expected  = "
      (TyFunc (TyCon int ()) (TyVar a))"
    in 
    (prog, expected, "recursive-fun")
  in
  let test3 = 
    let prog = "
      let rec test n =
          match n with
          | 0 -> 1
          | 1 -> 1
          | _ -> (test (n - 2)) + (test (n - 1))
      ;;"
    in
    let expected  = "
      (TyFunc (TyCon int ()) (TyCon int ()))"
    in 
    (prog, expected, "fib")
  in
  let test4 = 
    let prog = "
      let rec test (f : int -> int) (n : int) =
          match n with
          | 0 -> Nil
          | _ -> Cons(f n, (test f) (n - 1))
      ;;"
    in
    let expected  = "
      (TyFunc (TyFunc (TyCon int ()) (TyCon int ()))
        (TyFunc (TyCon int ()) (TyCon list ((TyCon int ())))))"
    in 
    (prog, expected, "rangef")
  in
  let test5 =
    let prog = "
      let rec test f lst =
          match lst with
          | Nil -> Nil
          | Cons(x, xs) ->
              let fxs = (test f) xs in
              Cons(f x, fxs)
      ;;"
    in
    let expected  = "
      (TyFunc (TyFunc (TyVar b) (TyVar a))
        (TyFunc (TyCon list ((TyVar b))) (TyCon list ((TyVar a)))))"
    in
    (prog, expected, "map")
  in
  let test6 =
    let prog = "
      let rec test pred lst =
          match lst with
          | Nil -> Nil
          | Cons(x, xs) ->
              let tl = (test pred) xs in
              if pred x then Cons(x, tl) else tl
      ;;"
    in
    let expected = "
      (TyFunc (TyFunc (TyVar a) (TyCon bool ()))
        (TyFunc (TyCon list ((TyVar a))) (TyCon list ((TyVar a)))))"
    in
    (prog, expected, "filter")
  in
  let test7 = 
    let prog = "
      let rec test f init lst =
          match lst with
          | Nil -> init
          | Cons(x, xs) -> (f x) (((test f) init) xs)
      ;;"
    in
    let expected = "
      (TyFunc (TyFunc (TyVar b) (TyFunc (TyVar a) (TyVar a)))
        (TyFunc (TyVar a) (TyFunc (TyCon list ((TyVar b))) (TyVar a))))"
    in
    (prog, expected, "foldr")
  in
  let test8 =
    let prog = "
      let rec test f init lst =
          match lst with
          | Nil -> init
          | Cons(x, xs) -> (f x) (((test f) init) xs)
      ;;
      let sum = (test (fun x y -> x + y)) 0 ;;"
    in
    let expected = "
      (TyFunc (TyFunc (TyVar b) (TyFunc (TyVar a) (TyVar a)))
        (TyFunc (TyVar a) (TyFunc (TyCon list ((TyVar b))) (TyVar a))))"
    in
    (prog, expected, "foldr-with-call-site")
  in
  [
    test1;
    test2;
    test3;
    test4;
    test5;
    test6;
    test7;
    test8;
  ]
;;

let make_tycheck_binding_test (prog, expected, name) =
  name >:: (fun _ -> test_tycheck_binding name prog expected)
;;

let suite =
  let parser_suite = 
    "parser" >::: [
      "parse_test1" >:: parse_test1;
      "parse_test2" >:: parse_test2;
      "parse_test3" >:: parse_test3;
      "parse_test4" >:: parse_test4;
      "parse_test5" >:: parse_test5;
      "parse_test6" >:: parse_test6;
      "parse_test7" >:: parse_test7;
    ]
  in
  let tycheck_suite =
    "tycheck" >::: List.map tycheck_binding_tests make_tycheck_binding_test
  in
  "suite" >::: [parser_suite; tycheck_suite]
;;

let () =
  run_test_tt_main suite
;;
