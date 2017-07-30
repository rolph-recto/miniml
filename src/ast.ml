open Core.Std
open Sexplib.Std

type name = string [@@deriving sexp]

(* record field declarations *)
type tyfield = {
  name: name;
  tyname: tyname;
} [@@deriving sexp]
  
(* type names *)
and tyname =
  | TyCon of name * tyname list
  | TyProd of tyname list
  | TyRec of tyfield list
  | TyVar of name
  | TyFunc of tyname * tyname
  [@@deriving sexp] ;;

(* value constructors *)
type condef =
  | ConDef of name * tyname
  | ConDefEmpty of name
  [@@deriving sexp] ;;

(* type definitions *)
type tydef =
  | TyName of name * name list * tyname
  (* these must be type constructors with tyvars *)
  | TySum of name * name list * condef list
  [@@deriving sexp]

type id =
  | Id of name
  | IdWithType of name * tyname
  [@@deriving sexp]

let name_of_id (id : id) : name =
  match id with
  | Id(name) -> name
  | IdWithType(name,t) -> name
;;

let type_of_id (id : id) : tyname option =
  match id with
  | Id(name) -> None
  | IdWithType(name,t) -> Some(t)
;;

type pattern = 
  | PatCon of name * pattern 
  | PatConEmpty of name
  | PatTuple of pattern list
  | PatUnit
  | PatInt of int
  | PatFloat of float
  | PatStr of string
  | PatBool of bool
  | PatVar of string
  | PatWildcard
  [@@deriving sexp]

type case = { match_pat: pattern; body: expr } [@@deriving sexp]
and field = { name: name; value: expr } [@@deriving sexp]
and expr =  
  | IntLit of int
  | FloatLit of float
  | StrLit of string
  | BoolLit of bool
  | Unit
  | Var of name
  | Tuple of expr list (* list must be non-empty, otherwise it's a unit *)
  | Rec of field list
  | Field of expr * name
  | Con of name * expr
  | ConEmpty of name
  | Let of id * expr * expr
  | Match of expr * case list
  | Cond of expr * expr * expr
  | Func of id * expr
  | App of expr * expr
  | Fix of expr
  [@@deriving sexp]

type progdef =
  | Binding of id * expr
  | Typedef of tydef
  [@@deriving sexp]

(* base/primitive types *)
let type_int     = TyCon("int", [])
let type_float   = TyCon("float", [])
let type_string  = TyCon("string", [])
let type_bool    = TyCon("bool", [])
let type_unit    = TyCon("unit", [])

type interp_val = 
  | IntVal of int
  | FloatVal of float
  | StrVal of string
  | BoolVal of bool
  | UnitVal
  | TupleVal of interp_val list
  | RecVal of field_vals
  | ConVal of name * interp_val
  | ConEmptyVal of name
  | ClosureVal of interp_env * name * expr
  | SysClosureVal of (interp_val -> (interp_val, interp_err) Result.t)
  [@@deriving sexp]
and field_vals = (name * interp_val) list
and interp_err =
  | UnboundVar of name
  | FieldNotFound of interp_val * name
  | PredicateNotBool of interp_val
  | ApplyNotFunc of interp_val
  | UnmatchedVal of interp_val
  | UnexpectedArg of interp_val
  | UserException of string
  [@@deriving sexp]
and interp_env = interp_val String.Map.t
