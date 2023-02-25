(* open OUnit2
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
         ( "it should be able to tostrings variable decl list" >:: fun _ ->
           let astNode =
             [ VariableDecl (Identifier "a"); VariableDecl (Identifier "b") ]
           in
           assert_equal
             ~printer:(fun x -> x)
             "[\n\
             \  (VariableDecl\n\
             \    (Identifier\n\
             \      (\"a\")));\n\
             \  (VariableDecl\n\
             \    (Identifier\n\
             \      (\"b\")))]"
             (Lalala.VariableDecl.to_strings astNode 0) );
       ] *)
