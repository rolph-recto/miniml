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
  | FieldNotFound of tyname * name
  | DuplicateField of tyname * name
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
 * free vars with fresh vars. this allows for let-generalization by
 * instantiating fresh vars for a binding's type scheme at every call site
 *)
let lookupEnv (st : infer_st) (env : tyenv) (x : name) : tyname = 
  match Map.find env x with
  | None    -> type_error (UnboundVar(x))
  | Some(t) -> let t' = instantiate st t in t'
;;

let add_constraint (st : infer_st) (t1 : tyname) (t2 : tyname) : unit = 
  st.constraints <- (t1,t2)::(st.constraints)
;;

let field_compare (f1 : tyfield) (f2 : tyfield) : int =
  String.compare (f1.name) (f2.name)
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
    let tyfields  = List.map fields field_to_tyfield in
    let tyfields' = List.sort field_compare tyfields in
    TyRec(tyfields')

  (* a limitation here: we assert that the type of `r` can be inferred to be a
   * record without unification; otherwise we throw an ambiguous field error.
   * there is no way for regular Hindley-Milner to determine the type of a
   * record without annotations; it would need to be extended with some kind of
   * subtyping (i.e. to assert that some record has *at least* some field)
   *)
  | Field(r, name) -> 
    let tr = infer st env r in
    begin match tr with
    | TyRec(tyfields) ->
        begin match List.filter tyfields (fun tf -> tf.name = name) with
        | []  -> type_error (FieldNotFound(tr, name))
        | [t] -> t.tyname
        | _   -> type_error (DuplicateField(tr, name))
        end
    | t -> type_error (FieldNotFound(t, name))
    end

  | Let(x, v, body) ->
    let get_id_type id expr =
      match id with
      | IdWithType(name, t) -> begin
        let tv = infer st env v in
        add_constraint st tv t;
        t
        end
      | Id(name) -> infer st env expr
    in
    let tv    = get_id_type x v in
    let sc    = generalize env tv in
    let name  = name_of_id x in
    let env'  = Map.add env name sc in
    infer st env' body

  | Match(e, cases) -> type_error (UnboundVar("lol"))
    (*
    let infer_case_body_type c =

    in
    let rec gen_body_constraints tbs =
      match tbs with
      | []          -> []
      | [_]         -> []
      | t1::t2::tl  ->
        add_constraint st t1 t2;
        gen_body_constraints (t2::tl)
    in
    let te  = infer st env e in
    let tcs = List.map cases (fun c -> pat_infer st env c.match_pat) in
    List.iter tcs (add_constraint st te);
    let tbs = List.map cases infer_case_body_type in
    *)

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
    let tsvar         = ([], tvar) in
    let env'          = Map.add env name tsvar in
    let tbody         = infer st env' body in
    TyFunc(tvar, tbody)

  | App(f, arg) ->
    let tf    = infer st env f in
    let targ  = infer st env arg in
    let tout  = freshVar st in
    add_constraint st tf (TyFunc(targ, tout));
    tout

and pat_infer (st : infer_st) (env : tyenv) (pat : pattern) : tyname =
  type_error (UnboundVar("lol"))
;;

let rec unify (s : subst) (t1 : tyname) (t2 : tyname) : subst =
  match (t1, t2) with
  | (TyVar(v), t) -> bind s v t

  | (t, TyVar(v)) -> bind s v t

  | (TyFunc(f1,a1), TyFunc(f2,a2)) -> unifyMany s [f1;a1] [f2;a2]

  | (TyProd(p1), TyProd(p2)) -> unifyMany s p1 p2

  | (TyRec(fs1), TyRec(fs2)) ->
    let fs1' = List.sort field_compare fs1 in
    let fs2' = List.sort field_compare fs2 in
    begin match List.zip fs1' fs2' with
    | Some(z) ->
      let same_count =
        List.filter z (fun (f1,f2) -> f1.name = f2.name) |> List.length
      in
      if same_count = List.length fs1' && same_count = List.length fs2'
      then begin
        let (fs1'', fs2'') =
          List.map z (fun (f1,f2) -> (f1.tyname, f2.tyname)) |> List.unzip
        in
        unifyMany s fs1'' fs2''
      end
      else type_error (UnificationFail(t1,t2))

    | None -> type_error (UnificationFail(t1,t2))
    end

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

let check_type' (e : expr) : (tyname * constr list, tyerror) Result.t =
  try begin
    let st : infer_st = { count=0; constraints=[] } in
    let env : tyenv   = String.Map.empty in
    let t             = infer st env e in
    st.constraints    <- List.rev st.constraints;
    let subst         = solve null_subst st.constraints in
    let principal     = TynameSub.apply subst t in
    Ok(principal, st.constraints)
  end with
  | TypecheckError(tyerr) -> Error(tyerr)
;;

let check_type (e : expr) : (tyname, tyerror) Result.t =
  try begin
    let st : infer_st = { count=0; constraints=[] } in
    let env : tyenv   = String.Map.empty in
    let t             = infer st env e in
    (* reverse constraint list to make sure constraints are unified in the
     * correct order; otherwise the principal type will not be computed
     * properly
     * e.g. consider the reverse of the following constraints:
     * ((TyVar v0), (TyFunc (TyCon int ()) (TyVar v2)))
     * ((TyVar v2), (TyFunc (TyCon int ()) (TyVar v3)))
     *)
    st.constraints    <- List.rev st.constraints;
    let subst         = solve null_subst st.constraints in
    let principal     = TynameSub.apply subst t in
    Ok(principal)
  end with
  | TypecheckError(tyerr) -> Error(tyerr)
;;

