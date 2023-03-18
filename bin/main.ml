open Lexing
open Lalala.Apexlexer
open Lalala.Apexparser

let read_lines name : string list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let filename = Sys.argv.(1)
let result = read_lines filename
let withnewlines = List.map (fun a -> a ^ "\n") result
let fileconcat = List.fold_left (fun a b -> a ^ b) "" withnewlines;;

print_string fileconcat

let buffer = from_string fileconcat
let ast = compilationUnit read_token buffer
let java = Lalala.Transpiler.transpile ast
let tosavelocation = Sys.argv.(2)
let tostringed = Lalala.JavaPrinter.to_string java;;
print_string tostringed;;
let oc = open_out tosavelocation;;

Printf.fprintf oc "%s\n" tostringed;;
close_out oc;;

(* let buffer = from_string "@IsTest public class AppTest {}";;

   buffer;;

   let result = Printf.sprintf "\n%d\n" (Lexing.lexeme_start buffer);;

   let () = print_string result *)
