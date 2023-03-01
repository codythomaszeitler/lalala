open OUnit2
open Lalala.Location
open Lalala.Ast
open Lalala.ApexIdentifier

let suite =
  "ApexClassDeclaration"
  >::: [
         ( "it should be able to pretty print an apex class decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let ast =
             ApexClassDeclaration
               (no_loc, ApexIdentifier (no_loc, "AnotherTestClass"), [])
           in
           pr_compilation_unit formatter ast;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexClassDeclaration{\n\
             \  id=(ApexIdentifier{\n\
             \       name=\n\
             \         \"AnotherTestClass\"\n\
             \       loc=\n\
             \         (Location)});\n\
             \  class_body_decls=[ ]\n\
             \  location=(Location)})" (Buffer.contents buffer) );
       ]
