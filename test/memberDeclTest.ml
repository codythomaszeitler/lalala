open OUnit2

let suite =
  "test suite for member decl ast"
  >::: [
         ( "tostring for the modifiers multiple args" >:: fun _ ->
           let ast =
             Lalala.MemberDecl.MethodDeclaration
               ( ApexType (Identifier "int"),
                 Identifier "getA",
                 [ ReturnStmt (Primary (Identifier "a")) ] )
           in
           assert_equal
             ~printer:(fun x -> x)
             "(MethodDeclaration\n\
             \  (ApexType\n\
             \    (Identifier\n\
             \      (\"int\"))),\n\
             \  (Identifier\n\
             \    (\"getA\")),\n\
             \  [\n\
             \    (ReturnStmt\n\
             \      (Primary\n\
             \        (Identifier\n\
             \          (\"a\")))))])"
             (Lalala.MemberDecl.to_string ast 0) );
       ]
