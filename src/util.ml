(* util.ml
* utility functions
*)

open Core.Std
open Ast

let rec sprint_tyname (tyname : tyname) : string =
  match tyname with
  | TyCon(name, args) -> 
    begin match args with
    | [] -> name
    | _ ->
      let sargs = List.map args (sprint_tyname) |> String.concat ~sep:"," in
      sprintf "%s[%s]" name sargs
    end
    
  | TyProd(prods) ->
    let sprods = List.map prods (sprint_tyname) |> String.concat ~sep:" * " in
    sprintf "(%s)" sprods
  
  | TyRec(tyfields) ->
    let sprint_tyfield (tyfield : tyfield) =
      sprintf "%s : %s;" tyfield.name (sprint_tyname tyfield.tyname)
    in
    let styfields = List.map tyfields sprint_tyfield |> String.concat ~sep:" " in
    sprintf "{ %s }" styfields

  | TyVar(var) -> var

  | TyFunc(t1, t2) ->
      let st1 = sprint_tyname t1 in
      let st2 = sprint_tyname t2 in
      sprintf "(%s -> %s)" st1 st2
  ;;

  let sprint_condef (condef : condef) : string =
    match condef with
    | ConDef(name, arg) -> sprintf "%s of %s" name (sprint_tyname arg)
    | ConDefEmpty(name) -> name
  ;;

  let sprint_tydef (tydef : tydef) : string =
    match tydef with
    | TyName(name, args, def) ->
      begin match args with
      | []  -> sprintf "type %s = %s" name (sprint_tyname def)
      | _   -> 
        let sargs = String.concat ~sep:"," args in
        sprintf "type %s[%s] = %s" name sargs (sprint_tyname def)
      end
    | TySum(name, args, cons) ->
      let sprint_cons cons =
        List.map cons (fun c -> c |> sprint_condef |> sprintf "  | %s")
        |> String.concat ~sep:"\n"
      in
      begin match args with
      | []  -> sprintf "type %s =\n" name
      | _   ->
        let sargs = String.concat ~sep:"," args in
        sprintf "type %s[%s] =\n%s" name sargs (sprint_cons cons)
      end
;;

let sprint_id (id : id) : string =
  match id with
  | Id(name) -> name
  | IdWithType(name, tyname) -> sprintf "(%s : %s)\n" name (sprint_tyname tyname)
;;

let rec sprint_expr (e : expr) : string =
  match e with
  | IntLit(n) -> string_of_int n

  | FloatLit(n) -> string_of_float n

  | StrLit(s) -> sprintf "\"%s\"" s

  | BoolLit(b) -> if b then "true" else "false"

  | Unit -> "()"

  | Var(v) -> v

  | Tuple(exprs) ->
    let sexprs = List.map exprs sprint_expr |> String.concat ~sep:"," in
    sprintf "(%s)" sexprs

  | Rec(fields) ->
    let sprint_field field =
      sprintf "%s = %s;" field.name (sprint_expr field.value)
    in
    let sfields = List.map fields sprint_field |> String.concat ~sep:" " in
    sprintf "{ %s }" sfields

  | Field(r, name) ->
    sprintf "%s.%s" (sprint_expr r) name
  
  | Con(name, arg) ->
    sprintf "(%s %s)" name (sprint_expr arg)
  
  | ConEmpty(name) -> name

  | Let(id, arg, body) ->
    sprintf "let %s = %s in\n%s" (sprint_id id) (sprint_expr arg) (sprint_expr body)

  | Match(x, cases) ->
    let scases = List.map cases sprint_case |> String.concat ~sep:"\n" in
    sprintf "match %s with\n%s" (sprint_expr x) scases

  | Cond(pred, t, e) ->
    sprintf "if %s then %s else %s" (sprint_expr pred) (sprint_expr t) (sprint_expr e)

  | Func(arg, body) ->
    sprintf "(fun %s -> %s)" (sprint_id arg) (sprint_expr body)

  | App(f, arg) ->
    sprintf "(%s %s)" (sprint_expr f) (sprint_expr arg)

  | Fix(f) ->
    sprintf "fix of %s" (sprint_expr f)

and sprint_case (case : case) : string =
    sprintf "| %s -> %s" (sprint_pattern case.match_pat) (sprint_expr case.body)

and sprint_pattern (p : pattern) : string =
    match p with
    | PatCon(name, arg) ->
      sprintf "(%s %s)" name (sprint_pattern arg)

    | PatConEmpty(name) -> name

    | PatTuple(pats) ->
      let spats = List.map pats sprint_pattern |> String.concat ~sep:"," in
      sprintf "(%s)" spats

    | PatUnit -> "()"

    | PatInt(n) -> string_of_int n

    | PatFloat(n) -> string_of_float n

    | PatStr(s) -> sprintf "\"%s\"" s

    | PatBool(b) -> if b then "true" else "false"

    | PatVar(v) -> v

    | PatWildcard -> "_"
;;

let rec sprint_interp_val (v : interp_val) : string =
  match v with
  | IntVal(n) -> string_of_int n

  | FloatVal(n) -> string_of_float n

  | StrVal(s) -> sprintf "\"%s\"" s

  | BoolVal(b) -> if b then "true" else "false"

  | UnitVal -> "()"

  | TupleVal(vals) ->
    let svals = List.map vals sprint_interp_val |> String.concat ~sep:"," in
    sprintf "(%s)" svals

  | RecVal(fieldvals) ->
    let sprint_fieldval (name, value) = 
      sprintf "%s = %s;" name (sprint_interp_val value)
    in
    let sfields = List.map fieldvals sprint_fieldval |> String.concat ~sep:" " in
    sprintf "{ %s }" sfields

  | ConVal(name, arg) ->
    sprintf "(%s %s)" name (sprint_interp_val arg)
  
  | ConEmptyVal(name) -> name

  | ClosureVal(_, arg, body) ->
    sprintf "(fun %s -> %s)" arg (sprint_expr body)

  | SysClosureVal(_) -> "(fun)"
;;

let sprint_interp_err (err : interp_err) : string =
  match err with
  | UnboundVar(name) -> sprintf "unbound variable '%s'" name

  | FieldNotFound(r, name) -> sprintf "field %s not in %s" name (sprint_interp_val r)

  | PredicateNotBool(v) ->
    sprintf "%s is not a bool, cannot use as predicate" (sprint_interp_val v)

  | ApplyNotFunc(f) -> sprintf "%s is not a function" (sprint_interp_val f)

  | UnmatchedVal(v) -> sprintf "unmatched value: %s" (sprint_interp_val v)

  | UnexpectedArg(v) -> sprintf "unexpected argument: %s" (sprint_interp_val v)

  | UserException(msg) -> sprintf "user exception: %s" msg
;;

let sprint_env (env : interp_env) : string =
  let sprint_bind (var, varval) =
    sprintf "%s => %s\n" var (varval |> sprint_interp_val)
  in
  List.map (env |> Map.to_alist) sprint_bind |> String.concat
;;

let print_env env = printf "%s\n" (sprint_env env)