open OUnit2
open Lalala.ApexVariableDecl
open Lalala.Location

let suite =
  "ApexVariableDecl"
  >::: [
         ( "it should be able to pretty print a variable decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let variableDecl =
             ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "a"))
           in
           Lalala.ApexVariableDecl.pr_variable_decl formatter variableDecl;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexVariableDecl{\n\
             \                 id=(ApexIdentifier{\n\
             \                                   name=\"a\"\n\
             \                                   loc=(Location)});\n\
             \                 loc=(Location)})" (Buffer.contents buffer) );
       ]
