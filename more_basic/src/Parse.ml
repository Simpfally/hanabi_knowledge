open Printf

let print_error lexer =
  let position = Lexing.lexeme_start_p lexer in
  let line = position.Lexing.pos_lnum
  and char = position.Lexing.pos_cnum - position.Lexing.pos_bol in
  sprintf "at line %d, column %d: syntax error." line char

(* get data from a file or standard input and pass it through the lexer *)
let input_channel name =
  let input = if name <> "-" then open_in name else stdin in
  let close = if name <> "-" then (fun () -> close_in input) else (fun () -> ()) in
  let lexer = Lexing.from_channel input in
  lexer, close

(* define a module *)
module TableBased =
struct
(*  open Lexing*)

(* A short name for the incremental parser API. *)
module I = Parser.MenhirInterpreter
module G = MenhirLib.General

exception ParserError of string

(* disclaimer : c/c from Abdallah *)
(* Adapted from F.Pottier code in CompCert. *)
(* Hmm... The parser is in its initial state. Its number is usually 0. This is a BIG HACK. TEMPORARY *)
let stack checkpoint = match checkpoint with
  | I.HandlingError env -> Lazy.force (I.stack env)
  | _ -> assert false (* this cannot happen, F. Pottier promises *)
let state checkpoint : int = match stack checkpoint with
  | G.Nil -> 0
  | G.Cons (I.Element (s, _, _, _), _) -> I.number s

(* called if lexer succeed? *)
let succeed v = v

(* called if lexer fail or parse too? *)
let fail lexbuf checkpoint =
  let herror = ParserMessages.message (state checkpoint) in
  (*let herror = ParsingErrors.message (state checkpoint) in*)
  let position = print_error lexbuf in
  let message = sprintf "%s %s" position herror in
  raise (ParserError message)

(* do something about lexing *)
let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result

(* load file, lexer and parser *)
let from_file name =
  let (lexer, close) = input_channel name in
  let e =
    try loop lexer (Parser.Incremental.record lexer.Lexing.lex_curr_p) with
    | Lexer.Error msg -> close (); invalid_arg (sprintf "Lexer error. In file %s, %s" name msg)
    | ParserError msg -> close (); invalid_arg (sprintf "Parser error. In file %s, %s" name msg)
    | exn -> close (); raise exn in
  close ();
  e

end

include TableBased
