{
open Lexing
open Apexparser
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let int = '-'? digit+  (* regex for integers *)
let ws    = [' ' '\t']
let iden =  ['a'-'z''A'-'Z']['a'-'z''A'-'Z''$''_']+


rule read_token = 
    parse 
    | ws {read_token lexbuf}
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | "+" { PLUS }
    | "*" { MULT }
    | "abstract" {ABSTRACT}
    | "after" {AFTER}
    | "public" {PUBLIC}
    | "{" {LEFT_BRACE}
    | "}" {RIGHT_BRACE}
    | "class" {CLASS}
    | iden {ID (Lexing.lexeme lexbuf)}
    | eof {EOF}
    | _ {ID (Lexing.lexeme lexbuf)}
