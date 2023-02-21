open OUnit2

let suite =
  "test suite for class body decl ast"
  >::: [
         ( "to string for class body decl" >:: fun _ ->
           let ast =
             Lalala.ClassBodyDecl.ClassBodyDeclaration
               ( Public,
                 MethodDeclaration
                   ( ApexType (Identifier "int"),
                     Identifier "getA",
                     [ ReturnStmt (Primary (Identifier "a")) ] ) )
           in
           assert_equal
             ~printer:(fun x -> x)
             "(ClassBodyDeclaration\n\
             \  (Public),\n\
             \  (MethodDeclaration\n\
             \    (ApexType\n\
             \      (Identifier\n\
             \        (\"int\"))),\n\
             \    (Identifier\n\
             \      (\"getA\")),\n\
             \    [\n\
             \      (ReturnStmt\n\
             \        (Primary\n\
             \          (Identifier\n\
             \            (\"a\")))))]))"
             (Lalala.ClassBodyDecl.to_string ast 0) );
       ]
