{
open Lexing
open Miniml_parser

exception SyntaxError of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let whitespace = [' ' '\t']+
let newline    = '\r' | '\n' | "\r\n"
let digit      = ['0'-'9']
let int_lit    = '-'? digit+
let frac       = '.' digit*
let exp        = ['e' 'E'] ['-' '+']? digit+
let float_lit  = digit frac? exp?
let id         = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let con        = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let tyvar      = ''' ['a'-'z']+
let binop      = ['<' '>' '=' '?' '@' '#' '$' '%' '^' '~' '&' '+' '/' '|' '.' '-']+

rule read =
  parse
  | whitespace              { read lexbuf }

  | newline                 { next_line lexbuf; read lexbuf }

  | '"'                     { read_string (Buffer.create 17) lexbuf }

  | "true"                  { TRUE }

  | "false"                 { FALSE }

  | "let"                   { LET }

  | "match"                 { MATCH }

  | "with"                  { WITH }

  | '|'                     { PIPE }

  | "of"                    { OF }

  | '*'                     { STAR }

  | '.'                     { PERIOD }

  | "->"                    { RARROW }

  | "begin"                 { BEGIN }

  | "end"                   { END }

  | '('                     { LPAREN }

  | ')'                     { RPAREN }

  | ','                     { COMMA }

  | ':'                     { COLON }

  | "type"                  { TYPE }

  | "="                     { EQ }

  | '{'                     { LBRAC }

  | '}'                     { RBRAC }

  | ';'                     { SCOLON }

  | "if"                    { IF }

  | "then"                  { THEN }

  | "else"                  { ELSE }

  | "fun"                   { FUN }

  | "in"                    { IN }

  | int_lit                 { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | float_lit               { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }

  | id                      { ID (Lexing.lexeme lexbuf) }

  | con                     { CON (Lexing.lexeme lexbuf) }

  | tyvar                   { TYVAR (Lexing.lexeme lexbuf) }

  | '_'                     { UNDERSCORE }

  (* binop should be the last regex because it's incredibly general *)
  | binop                   { BINOP (Lexing.lexeme lexbuf) }

  | eof                     { EOF }

and read_string buf =
  parse
  | '"'                     { STRING (Buffer.contents buf) }

  | '\\' '/'                { Buffer.add_char buf '/'; read_string buf lexbuf }

  | '\\' '\\'               { Buffer.add_char buf '\\'; read_string buf lexbuf }

  | '\\' 'b'                { Buffer.add_char buf '\b'; read_string buf lexbuf }

  | '\\' 'f'                { Buffer.add_char buf '\012'; read_string buf lexbuf }

  | '\\' 'n'                { Buffer.add_char buf '\n'; read_string buf lexbuf }

  | '\\' 'r'                { Buffer.add_char buf '\r'; read_string buf lexbuf }

  | '\\' 't'                { Buffer.add_char buf '\t'; read_string buf lexbuf }

  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }

  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

  | eof { raise (SyntaxError ("String is not terminated")) }

