(* Lexer file*)
(* Base from https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html *)
{ (* Optional ocaml code *)

  (* open Parser *)
  module P = Parser
  module L = Lexing

  exception Error of string
  
  (* track tokens across line breaks *)
  let incr_linenum lexbuf =
    let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with L.pos_lnum = pos.L.pos_lnum + 1;
                   L.pos_bol = pos.L.pos_cnum }
  let print_error lexer =
    let position = L.lexeme_start_p lexer in
    let line = position.L.pos_lnum in
    let col = position.L.pos_cnum - position.L.pos_bol in
    let s = L.lexeme lexer in (* +1 to match vim ; %! flush buffer *)
    Printf.sprintf "at line %d, column %d: unexpected character__%s_" line (col+1) s

  let string_of_char c = Printf.sprintf "%c" c
}

let digit = ['0' - '9']
let integer = digit+
let space = [' ' '\t']+
(*let lcase = ['a' - 'z']
let ucase = ['A' - 'Z']
let letter = lcase | ucase | digit | ['_'] | ['-'] | ['+'] | ['<'] | ['>'] | ['*'] | ['/'] | ['='] | ['@'] | ['#']
let variable = ['?']letter+
*)
let newline = "\r\n" | ['\n' '\r']

rule token = parse
  (*| variable as v { let v' = Scanf.sscanf v "?%s" (fun s -> s) in P.VARIABLE v' } *)
  | "(*"        { comment lexbuf }
  | eof         { P.EOF } (* TODO "%" should be eof but doesn't work, also adding a newline to parsing create issues so \n% here *)
  | "PLAY"      { P.PLAY}
  | "DISCARD"   { P.DISCARD}
  | "COLORCLUE" { P.COLORCLUE}
  | "RANKCLUE"  { P.RANKCLUE}
  | newline     { incr_linenum lexbuf ; P.NEWLINE }
  (*| ';'         { P.SEMICOL }*)
  | ','         { P.COMMA }
  | ':'         { P.COLON }
  | integer as s { let i = int_of_string s in P.INTEGER i }
  | space       { token lexbuf } (* skip space *)
  | _           { raise (Error (print_error lexbuf)) }

and comment = parse
  | "*)"     { token lexbuf }
  | _        { comment lexbuf }
(*and line_comment = parse
  | ([^'\n']* '\n') { incr_linenum lexbuf; token lexbuf }
  | ([^'\n']* eof) { P.EOF }
  | _   { raise (Error (print_error lexbuf)) }
*)
