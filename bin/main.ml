open Lexing

let buffer = from_string "@IsTest public class AppTest {}";;

buffer;;

let result = Printf.sprintf "\n%d\n" (Lexing.lexeme_start buffer);;

let () = print_string result