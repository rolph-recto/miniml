(* typechecker for miniml
 * this closely follows the Hindley-Milner chapter of 
 * Stephen Diehl's Write You A Haskell
 *)
open Core.Std
open Ast
module SSet = String.Set

type tyscheme = (name list) * tyname
type tyenv    = tyscheme String.Map.t
type subst    = tyname String.Map.t
type constr   = (tyname * tyname)

type tyerror =
  | UnificationFail of tyname * tyname
  | UnificationFails of (tyname list) * (tyname list)
  | InfiniteType of name * tyname
  | UnboundVar of name
  [@@deriving sexp] ;;

exception TypecheckError of tyerror

let type_error (err : tyerror) = raise (TypecheckError err)

module type Sub = sig
  type t
  val apply : subst -> t -> t
  val free_vars : t ->  SSet.t
end

module rec TyfieldSub : Sub with type t = tyfield = struct
  type t = tyfield
  let apply s field = { field with tyname = TynameSub.apply s field.tyname }
  let free_vars field = TynameSub.free_vars field.tyname
end
and TynameSub : Sub with type t = tyname = struct
  type t = tyname

  let rec apply s t =
    match t with
    | TyCon(name, args) -> 
      let args' = List.map ~f:(apply s) args in
      TyCon(name, args')

    | TyProd(prod) ->
      let prod' = List.map ~f:(apply s) prod in
      TyProd(prod')

    | TyRec(fields) ->
      let fields' = List.map ~f:(TyfieldSub.apply s) fields in
      TyRec(fields')

    | TyVar var ->
      begin match Map.find s var with
      | None -> TyVar var
      | Some(t) -> t
      end

    | TyFunc(t1, t2) -> 
      let t1' = apply s t1 in
      let t2' = apply s t2 in
      TyFunc(t1', t2')
  ;;

  let rec free_vars t =
    match t with
    | TyCon(name, args) -> 
      List.map ~f:free_vars args |> SSet.union_list

    | TyProd(prod) ->
      List.map ~f:free_vars prod |> SSet.union_list

    | TyRec(fields) ->
      List.map ~f:TyfieldSub.free_vars fields |> SSet.union_list

    | TyVar var ->
      SSet.singleton var

    | TyFunc(t1, t2) -> 
      let fv1 = free_vars t1 in
      let fv2 = free_vars t2 in
      SSet.union fv1 fv2
  ;;
end
and TyschemeSub : Sub with type t = tyscheme = struct
    type t = tyscheme

    let apply s (fvs, t) =
      let s' = List.fold fvs ~init:s ~f:Map.remove in
      (fvs, TynameSub.apply s' t) 
    ;;

    let free_vars (fvs, t) =
      SSet.diff (TynameSub.free_vars t) (SSet.of_list fvs)
    ;;
end

module ConstrSub : Sub with type t = constr = struct
  type t = constr

  let apply s (t1, t2) =
    let t1' = TynameSub.apply s t1 in
    let t2' = TynameSub.apply s t2 in
    (t1, t2)
  ;;

  let free_vars (t1, t2) =
    let fvs1 = TynameSub.free_vars t1 in
    let fvs2 = TynameSub.free_vars t2 in
    SSet.union fvs1 fvs2
  ;;
end

module ListSub (S : Sub) : Sub with type t = S.t list = struct
  type t = S.t list

  let apply s ts = List.map ~f:(S.apply s) ts

  let free_vars ts =
    List.fold_right ts
      ~f:(fun t acc -> SSet.union acc (S.free_vars t))
      ~init:SSet.empty
end

module TynameListSub = ListSub(TynameSub)
module TyschemeListSub = ListSub(TyschemeSub)
module ConstrListSub = ListSub(ConstrSub)

(* sub functions *)
let null_subst : subst = String.Map.empty

let prefer_left ~key = function
  | `Both(x,y)  -> Some(x)
  | `Left(x)    -> Some(x)
  | `Right(y)   -> Some(y)
;;

let compose (s1 : subst) (s2 : subst) : subst = 
  let s2' = Map.map ~f:(TynameSub.apply s1) s2 in
  Map.merge s2' s1 prefer_left
;;

let occurs_check (v : name) (t : tyname) : bool =
  SSet.mem (TynameSub.free_vars t) v
;;

type infer_st = {
  mutable count: int;
  mutable constraints: constr list
}

let var_prefix = "v"

let fresh (st : infer_st) : name =
  let n = st.count in
  st.count <- st.count + 1;
  var_prefix ^ (Int.to_string n)
;;

let freshVar (st : infer_st) : tyname =
  let name = fresh st in
  TyVar(name)
;;

let instantiate (st : infer_st) (s : tyscheme) : tyname = 
  let (fvs, t)  = s in
  let fvs'      = List.map ~f:(fresh st |> (fun n -> TyVar n) |> const) fvs in
  let sub       = List.zip_exn fvs fvs' |> String.Map.of_alist_exn  in
  TynameSub.apply sub t
;;

let generalize (env : tyenv) (t : tyname) : tyscheme =
  let ftv_env = Map.data env |> TyschemeListSub.free_vars in
  let ftv     = TynameSub.free_vars t in
  let tvs     = SSet.diff ftv ftv_env |> SSet.to_list in
  (tvs, t)
;;

(* whenever we lookup a binding, "monomorphize" by instantiating
 * free vars with fresh vars. this allows for let-generalization *)
let lookupEnv (st : infer_st) (env : tyenv) (x : name) : tyname = 
  match Map.find env x with
  | None    -> type_error (UnboundVar(x))
  | Some(t) -> let t' = instantiate st t in t'
;;

let add_constraint (st : infer_st) (t1 : tyname) (t2 : tyname) : unit = 
  st.constraints <- (t1,t2)::st.constraints
;;

let rec infer (st : infer_st) (env : tyenv) (e : expr) : tyname =
  match e with
  | ILit(_)   -> type_int

  | FLit(_)   -> type_float

  | SLit(_)   -> type_string

  | BLit(_)   -> type_bool

  | Var(name) -> lookupEnv st env name

  | Unit      -> type_unit

  | Tuple(exprs) ->
    let tyexprs = List.map exprs (infer st env) in
    TyProd(tyexprs)

  | Rec(fields) ->
    let field_to_tyfield f =
      let t = infer st env f.value in
      { name=f.name; tyname=t }
    in
    let field_compare (f1 : tyfield) (f2 : tyfield) =
      String.compare (f1.name) (f2.name)
    in
    let tyfields  = List.map fields field_to_tyfield in
    let tyfields' = List.sort field_compare tyfields in
    TyRec(tyfields')

  | Field(record, name) -> type_error (UnboundVar("implement_field_infer"))

  | Let(id, body) -> type_error (UnboundVar("implement_let_infer"))

  | Match(e, cases) -> type_error (UnboundVar("implement_match_infer"))

  | Cond(pred, thenExpr, elseExpr) ->
    let typred = infer st env pred in
    let tythen = infer st env thenExpr in
    let tyelse = infer st env elseExpr in
    add_constraint st typred type_bool;
    add_constraint st tythen tyelse;
    tythen

  | Func(var, body) ->
    let get_var_type v =
      match v with
      | Id(name) -> (name, freshVar st)
      | IdWithType(name, t) -> (name, t)
    in
    let (name, tvar)  = get_var_type var in
    let tsvar         = generalize env tvar in
    let env'          = Map.add env name tsvar in
    let tbody         = infer st env' body in
    TyFunc(tvar, tbody)

  | App(f, arg) ->
    let tf    = infer st env f in
    let targ  = infer st env arg in
    let tout  = freshVar st in
    add_constraint st tf (TyFunc(targ, tout));
    tout
;;

let rec unify (s : subst) (t1 : tyname) (t2 : tyname) : subst =
  match (t1, t2) with
  | (TyVar(v), t) -> bind s v t

  | (t, TyVar(v)) -> bind s v t

  | (TyFunc(f1,a1), TyFunc(f2,a2)) -> unifyMany s [f1;a1] [f2;a2]

  | (TyProd(p1), TyProd(p2)) -> unifyMany s p1 p2

  | (TyRec(f1), TyRec(f2)) -> type_error (UnboundVar("unify_rec"))

  | (TyCon(n1,a1), TyCon(n2,a2)) ->
    if n1 = n2
    then unifyMany s a1 a2
    else type_error (UnificationFail(t1,t2))

  | _ ->
    if t1 = t2
    then null_subst
    else type_error (UnificationFail(t1,t2))

and bind (s : subst) (n : name) (t : tyname) : subst =
  match t with
  | TyVar(_)  -> s

  | _ -> 
    if occurs_check n t
    then type_error (InfiniteType(n,t))
    else Map.merge (String.Map.singleton n t) s prefer_left 

and unifyMany (s : subst) (l1 : tyname list) (l2 : tyname list) : subst =
  match (l1, l2) with
  | ([], []) -> null_subst

  | (t1::ts1, t2::ts2)  ->
    let s1    = unify s t1 t2 in
    let ts1'  = TynameListSub.apply s1 ts1 in
    let ts2'  = TynameListSub.apply s1 ts2 in
    let s2    = unifyMany s1 ts1' ts2' in
    compose s2 s1

  | _  -> type_error (UnificationFails(l1,l2))
;;

let rec solve (s : subst) (cons : constr list) : subst =
  match cons with
  | [] -> s
  | (t1, t2)::cs ->
    let s1 = unify s t1 t2 in
    solve (compose s1 s) (ConstrListSub.apply s1 cs)
;;

let check_type (e : expr) : (tyname, tyerror) Result.t =
  try begin
    let st : infer_st = { count=0; constraints=[] } in
    let env : tyenv   = String.Map.empty in
    let t             = infer st env e in
    let subst         = solve null_subst st.constraints in
    let principal     = TynameSub.apply subst t in
    Ok(principal)
  end with
  | TypecheckError(tyerr) -> Error(tyerr)
;;

