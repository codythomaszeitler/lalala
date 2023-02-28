open OUnit2
open Lalala.Stmt
open Lalala.Location
open Lalala.Expr
open Lalala.ApexLiteral

let suite =
  "Stmt"
  >::: [
         ( "it should be able to pretty print a return stmt" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let astNode =
             ReturnStmt (no_loc, Primary (no_loc, IntegerLiteral (no_loc, 100)))
           in
           pr_stmt formatter astNode;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ReturnStmt{\n\
             \            expr=(Primary {\n\
             \                           apexLiteral=(IntegerLiteral{\n\
             \                                                       int=100;\n\
             \                                                       \
              loc=(Location)\n\
             \                                                       });\n\
             \                           loc=(Location)\n\
             \                           });\n\
             \            location=(Location);})" (Buffer.contents buffer) );
         (* ( "it should be able to tostring a local var decl stmt" >:: fun _ ->
            let astNode =
              Lalala.Stmt.LocalVarDeclStmt
                (LocalVarDecl
                   ( Public,
                     ApexType (Identifier "int"),
                     [ VariableDecl (Identifier "a") ] ))
            in
            assert_equal
              ~printer:(fun x -> x)
              ""
              (Lalala.Stmt.to_string astNode 0) ); *)
       ]
