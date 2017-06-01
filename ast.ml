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
  | TyProd of tyname * tyname
  | TyRec of tyfield list
  | TyVar of name
  | TyFunc of tyname * tyname
  [@@deriving sexp] ;;

(* (value) constructors *)
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

type pattern = 
  | PatCon of string * pattern list
  | PatTuple of pattern list
  | PatUnit
  | PatILit of int
  | PatFLit of float
  | PatSLit of string
  | PatBLit of bool
  | PatVar of string
  | PatWildcard
  [@@deriving sexp]

type case = { match_pat: pattern; body: expr } [@@deriving sexp]
and field = { name: name; value: expr } [@@deriving sexp]
and expr =  
  | ILit of int
  | FLit of float
  | SLit of string
  | BLit of bool
  | Var of name
  | Unit
  | Tuple of expr list (* list must be non-empty, otherwise it's a unit *)
  | Rec of field list
  | Field of expr * name
  | Let of id * expr
  | Match of expr * case list
  | Cond of expr * expr * expr
  | Func of id * expr
  | App of expr * expr
  [@@deriving sexp]

type progdef =
  | Expr of expr
  | Typedef of tydef
  [@@deriving sexp]
