open OUnit2

let suite =
  "test suite for statement ast"
  >::: [
         ( "it should be able to tostring return stmt" >:: fun _ ->
           let astNode = Lalala.Stmt.ReturnStmt (Primary (Identifier "a")) in
           assert_equal
             ~printer:(fun x -> x)
             "(ReturnStmt\n  (Primary\n    (Identifier\n      (\"a\")))))"
             (Lalala.Stmt.to_string astNode 0) );
         ( "it should be able to tostring a local var decl stmt" >:: fun _ ->
           let astNode =
             Lalala.Stmt.LocalVarDeclStmt
               (LocalVarDecl
                  ( Public,
                    ApexType (Identifier "int"),
                    [ VariableDecl (Identifier "a") ] ))
           in
           assert_equal
             ~printer:(fun x -> x)
             "(LocalVarDecl\n\
             \  (LocalVarDecl\n\
             \    (Public),\n\
             \    (ApexType\n\
             \      (Identifier\n\
             \        (\"int\"))),\n\
             \    [\n\
             \      (VariableDecl\n\
             \        (Identifier\n\
             \          (\"a\")))]))"
             (Lalala.Stmt.to_string astNode 0) );
       ]
