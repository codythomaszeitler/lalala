(* open OUnit2

let suite =
  "test suite for class decl ast"
  >::: [
         ( "tostring for class decl" >:: fun _ ->
           let ast =
             Lalala.ClassDecl.ClassDeclaration
               ( Identifier "AnotherTestClass",
                 [
                   ClassBodyDeclaration
                     ( Public,
                       FieldDeclaration
                         ( ApexType (Identifier "int"),
                           [ VariableDecl (Identifier "a") ] ) );
                   ClassBodyDeclaration
                     ( Public,
                       MethodDeclaration
                         ( ApexType (Identifier "int"),
                           Identifier "getA",
                           [ ReturnStmt (Primary (Identifier "a")) ] ) );
                 ] )
           in
           assert_equal
             ~printer:(fun x -> x)
             "(ClassDeclaration\n\
             \  (Identifier\n\
             \    (\"AnotherTestClass\")),\n\
             \  [\n\
             \    (ClassBodyDeclaration\n\
             \      (Public),\n\
             \      (FieldDeclaration\n\
             \        (ApexType\n\
             \          (Identifier\n\
             \            (\"int\"))),\n\
             \    [\n\
             \      (VariableDecl\n\
             \        (Identifier\n\
             \          (\"a\")))]));\n\
             \      (ClassBodyDeclaration\n\
             \        (Public),\n\
             \        (MethodDeclaration\n\
             \          (ApexType\n\
             \            (Identifier\n\
             \              (\"int\"))),\n\
             \          (Identifier\n\
             \            (\"getA\")),\n\
             \          [\n\
             \            (ReturnStmt\n\
             \              (Primary\n\
             \                (Identifier\n\
             \                  (\"a\")))))]))]))"
             (Lalala.ClassDecl.to_string ast 0) );
       ] *)
