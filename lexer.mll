{
open Lexing
open Parser
open Printf

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

let digit   = ['0'-'9']
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let lambda  = '\\'
let dot     = '.'
let lparen  = '('
let rparen  = ')'

let id      = ['a'-'z' 'A'-'Z']

rule read =
    parse
    | white   { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | lambda  { LAMBDA }
    | id      { ID (Lexing.lexeme lexbuf) }
    | dot     { DOT }
    | lparen  { LPAREN }
    | rparen  { RPAREN }
    | _       { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
    | eof     { EOF }

{
(* parser *)
let print_pos outx lexbuf =
    let pos = lexbuf.lex_curr_p in
        fprintf outx "%s:%d:%d" pos.pos_fname
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let unchecked_parse lexbuf =
    try Parser.prog read lexbuf with
        | SyntaxError msg ->
            fprintf stderr "%a: %s\n" print_pos lexbuf msg; (* impure! *)
            None
        | Parser.Error ->
            fprintf stderr "%a: syntax error\n" print_pos lexbuf;
            exit (-1)

let parse lexbuf =
    unchecked_parse lexbuf

let parse_file f =
    let inx = open_in f in
    let lexbuf = Lexing.from_channel inx in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f};
        
        let result = parse lexbuf in
          close_in inx; (* ick *)
          result
}