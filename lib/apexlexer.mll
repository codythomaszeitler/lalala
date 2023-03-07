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

let keyword_tbl = Hashtbl.create 64  

let _ = List.iter (fun (name, keyword) ->
    Hashtbl.add keyword_tbl name keyword) [
    "public", PUBLIC;
    "after", AFTER; 
    "abstract", ABSTRACT;
    "private", PRIVATE;
    "static", STATIC;
    "return", RETURN;
    "class", CLASS;
  ]}

let digit = ['0'-'9']
let int = '-'? digit+  (* regex for integers *)
let ws    = [' ' '\t' '\n']
let iden =  ['a'-'z''A'-'Z']['a'-'z''A'-'Z''$''_']+

rule read_token = 
    parse 
    | ws {read_token lexbuf}
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | "+" { PLUS }
    | "*" { MULT }
    | "@" {ATSIGN}
    | "{" {LEFT_BRACE}
    | "}" {RIGHT_BRACE}
    | "(" {LEFT_PAREN}
    | ")" {RIGHT_PAREN}
    | ';' {SEMI}
    | '.' {DOT}
    | ',' {COMMA}
    | '=' {ASSIGN}
    | eof {EOF}
    | iden as s {
        let idOrKeyword = String.lowercase_ascii s in 
        try Hashtbl.find keyword_tbl idOrKeyword
        with Not_found -> ID s
    }
    | _ {ID (Lexing.lexeme lexbuf)}
