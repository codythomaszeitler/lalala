open OUnit2
open Lalala.VariableDecl
open Lalala.Identifier

let suite =
  "test suite for variable decl apex ast"
  >::: [
         ( "it should be able to tostring variable decl" >:: fun _ ->
           let astNode = VariableDecl (Identifier "a") in
           assert_equal
             ~printer:(fun x -> x)
             "(VariableDecl\n  (Identifier\n    (\"a\")))"
             (Lalala.VariableDecl.to_string astNode 0) );
       ]
