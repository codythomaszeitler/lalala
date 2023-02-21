open OUnit2

let suite =
  "test suite for statement ast"
  >::: [
         ( "it should be able to tostring return stmt" >:: fun _ ->
           let astNode = Lalala.Stmt.ReturnStmt (Primary (Identifier "a")) in
           assert_equal
             ~printer:(fun x -> x)
             "(ReturnStmt\n\
             \  (Primary\n\
             \    (Identifier\n\
             \      (\"a\")))))"
             (Lalala.Stmt.to_string astNode 0) );
       ]
