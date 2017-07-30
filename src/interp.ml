(* interp.ml 
* reference interpreter for miniml *)

open Core.Std
open Ast

let print_env (env : interp_env) =
  let print_bind (var, varval) =
    printf "%s => %s\n" var (varval |> sexp_of_interp_val |> Sexp.to_string_hum)
  in
  List.iter (env |> Map.to_alist) print_bind;
;;

let rec unify_val (v : interp_val) (p : pattern) : field_vals Option.t = 
  let open Option in
  match (v, p) with
  | (IntVal vn, PatInt pn) when vn = pn  -> Some []

  | (FloatVal vn, PatFloat pn) when vn = pn -> Some []

  | (BoolVal vb, PatBool pb) when vb = pb -> Some []

  | (StrVal vs, PatStr ps) when vs = ps -> Some []

  | (UnitVal, PatUnit) -> Some []

  | (ConVal(cname, cval), PatCon(pname, pval)) when cname = pname ->
    unify_val cval pval

  | (ConEmptyVal cname, PatConEmpty pname) when cname = pname ->
    Some []

  | (_, PatVar pvar) -> Some [(pvar, v)]

  | (_, PatWildcard) -> Some []

  | (TupleVal tvs, PatTuple pts) ->
    List.zip tvs pts >>= fun pairs ->
    let unified_pairs = List.map pairs (fun (tv, pt) -> unify_val tv pt) in
    Option.all unified_pairs >>= fun nested_binds ->
    return (nested_binds |> List.concat)

  | _ -> None
;;

let rec interp (env : interp_env) (e : expr) : (interp_val, interp_err) Result.t =
  let open Result in
  match e with
  | IntLit(n)   -> return (IntVal n)

  | FloatLit(f) -> return (FloatVal f)

  | StrLit(s)   -> return (StrVal s)

  | BoolLit(b)  -> return (BoolVal b)

  | Unit        -> return UnitVal

  | Var(v) ->
    begin match (Map.find env v) with
    | Some(value) -> return value
    | None ->
      Error(UnboundVar v)
  end

  | Tuple(texprs) ->
    List.map texprs (interp env) |> Result.all >>= fun tres ->
    return (TupleVal tres)

  | Rec(fields) ->
    let interp_field field = 
      interp env field.value >>= fun fval ->
      return (field.name, fval)
    in
    List.map fields interp_field |> Result.all >>= fun fvals ->
    return (RecVal fvals)

  | Field(r, fname) ->
    interp env r >>= fun rval ->
    begin match rval with
    | RecVal(fvals) ->
      begin match List.Assoc.find fvals fname with
      | Some(fval) -> return fval
      | None -> Error(FieldNotFound(rval, fname))
      end

    | _ -> Error(FieldNotFound(rval, fname))
  end

  | Con(con_name, con_expr) -> 
    interp env con_expr >>= fun con_val ->
    return (ConVal(con_name, con_val))

  | ConEmpty(con_name) -> return (ConEmptyVal(con_name))

  | Let(id, vexpr, body) ->
    let var = name_of_id id in
    interp env vexpr >>= fun vval ->
    let env' = Map.add env var vval in
    interp env' body

  | Match(x, cases) ->
    let rec match_cases xval cs = 
      match cs with
      | []    -> Error(UnmatchedVal xval)
      | c::tc ->
        begin match unify_val xval c.match_pat with
        | None -> match_cases xval tc
        | Some(binds) ->
          let env' = List.fold binds ~init:env ~f:(fun acc (var, varval) -> Map.add acc var varval) in
          interp env' c.body
        end
    in
    interp env x >>= fun xval ->
    match_cases xval cases

  | Cond(pred, then_expr, else_expr) ->
    interp env pred >>= fun predval ->
    begin match predval with
    | BoolVal(bval) ->
      if bval then interp env then_expr else interp env else_expr

    | _ -> Error(PredicateNotBool predval)
    end
  
  | Func(arg, body) -> 
    let var = name_of_id arg in
    return (ClosureVal(env, var, body))

  (* evaluate eagerly *)
  | App(f, arg) ->
    interp env f >>= fun fval ->
    begin match fval with
    | ClosureVal(fenv, var, body) ->
      interp env arg >>= fun argval ->
      let env'  = Map.merge env fenv Typecheck.prefer_left in
      let env'' = Map.add env' var argval in
      (*
      printf "ENV:\n";
      print_env fenv;
      printf "FENV:\n";
      print_env fenv;
      printf "body: %s\n" (body |> Ast.sexp_of_expr |> Sexp.to_string_hum);
      printf "argval: %s\n" (body |> Ast.sexp_of_expr |> Sexp.to_string_hum);
      *)
      interp env'' body

    | SysClosureVal(f) ->
      interp env arg >>= fun argval ->
      f argval

    | _ -> Error(ApplyNotFunc fval)
    end

 | Fix(f) ->
    interp env (App(f, f))
;;

let rec interp_prog' (env : interp_env) (prog : progdef list) : (interp_val String.Map.t, interp_err) Result.t = 
  let open Result in
  let interp_progdef progdef =
    match progdef with
    | Binding(id, body) ->
      interp env body >>= fun bodyval ->
      let name = name_of_id id in
      return (name, bodyval)

    | Typedef(TyName(tyname, _, _)) -> Ok(tyname, UnitVal)

    | Typedef(TySum(tyname, _, _)) -> Ok(tyname, UnitVal)
  in
  match prog with
  | [] -> Ok String.Map.empty
  | p::tp ->
    interp_progdef p >>= fun (pname, pval) ->
    match pval with
    | UnitVal -> interp_prog' env tp
    | _ ->
      let env' = Map.add env pname pval in
      interp_prog' env' tp >>= fun tvals ->
      return (Map.add tvals pname pval)
;;

let binop_func
  (egress : interp_val -> ('a, interp_err) Result.t)
  (ingress : 'a -> interp_val)
  (f : 'a -> 'a -> 'a)
: interp_val =
  let open Result in
  let sf v1 = 
    egress v1 >>= fun x ->
    let sf2 v2 =
      egress v2 >>= fun y ->
      Ok(ingress (f x y))
    in
    Ok(SysClosureVal sf2)
  in
  SysClosureVal sf
;;

let egress_int = function
  | IntVal x -> Ok(x)
  | v -> Error(UnexpectedArg v)
;;
let ingress_int n = IntVal n

let egress_float = function
  | FloatVal x -> Ok(x)
  | v -> Error(UnexpectedArg v)
;;
let ingress_float n = FloatVal n

let egress_str = function
  | StrVal x -> Ok(x)
  | v -> Error(UnexpectedArg v)
;;
let ingress_str n = StrVal n

let egress_bool = function
  | BoolVal x -> Ok(x)
  | v -> Error(UnexpectedArg v)
;;
let ingress_bool n = BoolVal n

let binop_int f = binop_func egress_int ingress_int f
let binop_float f = binop_func egress_float ingress_float f
let binop_str f = binop_func egress_str ingress_str f
let binop_bool f = binop_func egress_bool ingress_bool f

let error_func =
  let f v1 =
    match v1 with
    | StrVal s -> Error(UserException s)
    | _ -> Error(UnexpectedArg v1)
  in
  SysClosureVal f
;;

let base_env =
  let open Result in
  let env = [
    "+", binop_int (+);
    "-", binop_int (-);
    "*", binop_int (fun x y -> x * y);
    "/", binop_int (/);
    "+.", binop_float (+.);
    "-.", binop_float (-.);
    "*.", binop_float (fun x y -> x *. y);
    "/.", binop_float (/.);
    "^", binop_str (^);
    "error", error_func;
  ] in
  String.Map.of_alist_exn env
;;

let interp_prog prog =
  interp_prog' base_env prog
;;