(* header *)
%{
  open Core.Std
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> CON
%token <string> TYVAR
%token <string> STRING
%token <string> BINOP
%token TRUE
%token FALSE
%token LET
%token MATCH
%token WITH
%token PIPE
%token OF
%token STAR
%token PERIOD
%token UNDERSCORE
%token RARROW
%token BEGIN
%token END
%token LPAREN
%token RPAREN
%token COMMA
%token COLON
%token TYPE
%token EQ
%token LBRAC
%token RBRAC
%token SCOLON
%token IF
%token THEN
%token ELSE
%token FUN
%token IN
%token EOF

%nonassoc binding_prec
%right    RARROW
%left     STAR
%left     BINOP
%right    app
%left     PERIOD

%start <Ast.progdef list> prog

%{
let build_let args e =
  match args with
  | []    -> e
  | _::_  -> List.fold (List.rev args) ~init:e ~f:(fun acc arg -> Func(arg, acc))
;;
%}

%%

(* production rules *)

tyfield:
  name = ID; COLON; tyname = tyname     { { name; tyname} };

tyfield_list:
  | tl = tyfield_list; hd = tyfield     { tl@[hd] }

  | v = tyfield                         { [v] }

tyname: 
  | args = tyname_list; tycon = ID      { TyCon(tycon, args) }

  | hd = tyname; STAR; tl = typrod_list { TyProd(hd::tl) }

  | LBRAC; fields = tyfield_list; RBRAC { TyRec(fields) }

  | v = TYVAR                           { TyVar(v) }

  | t1 = tyname; RARROW; t2 = tyname    { TyFunc(t1, t2) }

  | LPAREN; t = tyname; RPAREN          { t }
  ;

tyname_list:
  | tl = tyname_list; hd = tyname       { tl@[hd] }
  | v = tyname                          { [v] }
  | (* empty *)                         { [] }
  ;

typrod_list:
  | tl = typrod_list; STAR; hd = tyname   { tl@[hd] }
  | v = tyname                            { [v] }
  ;

tyvar_list:
  | tl = tyvar_list; hd = TYVAR         { tl@[hd] }

  | (* empty *)                         { [] }
  ;

tycon:
  | PIPE; con = CON; OF; ty = tyname      { ConDef(con, ty) }
  | PIPE; con = CON;                      { ConDefEmpty(con) }

tycon_list:
  | v = tycon                           { [v] }

  | tl = tycon_list; hd = tycon         { tl@[hd] }

tydef:
  | TYPE; args = tyvar_list; name = ID; EQ; cons = tycon_list
    { TySum(name, args, cons) }

  | TYPE; args = tyvar_list; name = ID; EQ; tyname = tyname
    { TyName(name, args, tyname) }
  ;

arg:
  | name = ID; COLON; tyname = tyname
    { IdWithType(name, tyname) }

  | name = ID;
    { Id(name) }

  | LPAREN; v = arg; RPAREN             { v }
  ;

arg_list:
  | (* empty *)                         { [] }

  | tl = arg_list; hd = arg             { tl@[hd] }
  ;

pattern:
  | name = CON; LPAREN; args = pat_list; RPAREN
    { PatCon(name, args) }

  | name = CON                          { PatCon(name, []) }

  | LPAREN; RPAREN                      { PatUnit }

  | v = INT                             { PatILit(v) }

  | v = FLOAT                           { PatFLit(v) }

  | v = STRING                          { PatSLit(v) }

  | v = TRUE                            { PatBLit(true) }

  | v = FALSE                           { PatBLit(false) }

  | v = ID                              { PatVar(v) }

  | v = UNDERSCORE                      { PatWildcard }
  ;

pat_list:
  | v = pattern                         { [v] }

  | tl = pat_list; COMMA; hd = pattern  { tl@[hd] }
  ;

expr_id:
  | v = ID                              { v }
  | LPAREN; v = BINOP; RPAREN           { v }
  | LPAREN; v = expr_id; RPAREN         { v }


expr:
  | v = INT                             { ILit(v) }

  | v = FLOAT                           { FLit(v) }

  | v = STRING                          { SLit(v) }

  | v = TRUE                            { BLit(true) }

  | v = FALSE                           { BLit(false) }

  | name = expr_id                      { Var(name) }

  | LPAREN; RPAREN                      { Unit }

  | LPAREN; lst = expr_list; RPAREN     { Tuple(lst) }

  | LBRAC; lst = field_list; RBRAC      { Rec(lst) }

  | name = CON; arg = expr              { Con(name, arg) }

  | name = CON                          { ConEmpty(name) }

  | LET; name = expr_id; args = arg_list; COLON; tyname = tyname; EQ; e = expr; IN; body = expr;
    {
      let v = build_let args e in
      Let(IdWithType(name, tyname), v, body)
    }
    %prec binding_prec

  | LET; name = expr_id; args = arg_list; EQ; e = expr; IN; body = expr
    {
      let v = build_let args e in
      Let(Id(name), v, body)
    }
    %prec binding_prec

  | IF; pred = expr; THEN; then_expr = expr; ELSE; else_expr = expr
    { Cond(pred, then_expr, else_expr) }

  | MATCH; v = expr; WITH; cases = case_list
    { Match(v, cases) }

  | FUN; args = arg_list; RARROW; body = expr
    { List.fold (List.rev args) ~init:body ~f:(fun acc arg -> Func(arg, acc)) }

  | r = expr; PERIOD; field = ID        { Field(r, field) }

  | LPAREN; e = expr; RPAREN            { e }

  | BEGIN; e = expr; END                { e }

  | f = expr; arg = expr                { App(f, arg) }

  (* mult has to be a special case because STAR is a special token
   * that is also used by product types *)
  | a1 = expr; op = STAR; a2 = expr     { App(App(Var("*"), a1), a2) }

  | a1 = expr; op = EQ; a2 = expr       { App(App(Var("="), a1), a2) }

  (* support infix binary operations *)
  | a1 = expr; op = BINOP; a2 = expr    { App(App(Var(op), a1), a2) } %prec app
  ;

binding:
  | LET; name = expr_id; args = arg_list; COLON; tyname = tyname; EQ; e = expr
    {
      let body = List.fold (List. rev args)
                  ~init:e ~f:(fun acc arg -> Func(arg, acc)) in
      (IdWithType(name, tyname), body)
    }
    %prec binding_prec

  | LET; name = expr_id; args = arg_list; EQ; e = expr
    {
      let body = List.fold (List.rev args)
                  ~init:e ~f:(fun acc arg -> Func(arg, acc)) in
      (Id(name), body)
    }
    %prec binding_prec

expr_list:
  | tl = expr_list; COMMA; hd = expr    { tl@[hd] }

  | v = expr                            { [v] }
  ;

field:
  name = ID; EQ; value = expr           { { name; value } };

field_list:
  | tl = field_list; SCOLON; hd = field { tl@[hd] }
  | v = field                           { [v] }
  ;

case:
  PIPE; match_pat = pattern; RARROW; body = expr
  { { match_pat; body } };

case_list:
  | tl = case_list; hd = case           { tl@[hd] }

  | v = case                            { [v] }
  ;

prog:
  | EOF { [] }

  | hd = binding; SCOLON; SCOLON; tl = prog
    { Binding(fst hd, snd hd)::tl }

  | hd = tydef; SCOLON; SCOLON; tl = prog
    { Typedef(hd)::tl }

  (* top-level bindings must have delimiters (or else we get weird parsing!)
  | hd = binding; tl = prog
    { Expr(hd)::tl }

  | hd = tydef; tl = prog
    { Typedef(hd)::tl }
  *)
  ;

