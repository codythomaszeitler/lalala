open OUnit2
open Lalala.LocalVarDecl
open Lalala.Modifier
open Lalala.ApexType
open Lalala.VariableDecl

let suite =
  "test suite for local var decl ast"
  >::: [
         ( "it should be able to tostring a local var decl" >:: fun _ ->
           let astNode =
             LocalVarDecl
               ( Public,
                 ApexType (Identifier "int"),
                 [ VariableDecl (Identifier "a") ] )
           in
           assert_equal
             ~printer:(fun x -> x)
             "(LocalVarDecl\n\
             \  (Public),\n\
             \  (ApexType\n\
             \    (Identifier\n\
             \      (\"int\"))),\n\
             \  [\n\
             \    (VariableDecl\n\
             \      (Identifier\n\
             \        (\"a\")))])"
             (Lalala.LocalVarDecl.to_string astNode 0) );
       ]
