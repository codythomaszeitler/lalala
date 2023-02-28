open OUnit2
open Lalala.Stmt
open Lalala.Location
open Lalala.Expr
open Lalala.ApexLiteral
open Lalala.ApexLocalVarDecl

let suite =
  "Stmt"
  >::: [
         ( "it should be able to pretty print a return stmt" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let astNode =
             ApexReturnStmt
               (no_loc, Primary (no_loc, IntegerLiteral (no_loc, 100)))
           in
           pr_stmt formatter astNode;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexReturnStmt{\n\
             \  expr=\n\
             \    (Primary{\n\
             \      apexLiteral=\n\
             \        (IntegerLiteral{\n\
             \          int=100;\n\
             \          loc=(Location)});\n\
             \      loc=(Location)});\n\
             \  location=(Location);})" (Buffer.contents buffer) );
         ( "it should be able to pretty print a apex local var decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let astNode =
             ApexLocalVarDeclStmt
               ( no_loc,
                 ApexLocalVarDecl
                   ( no_loc,
                     Public no_loc,
                     ApexTypeName (no_loc, ApexIdentifier (no_loc, "int")),
                     [ ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "a")) ]
                   ) )
           in
           pr_stmt formatter astNode;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexLocalVarDeclStmt{\n\
             \  local_var_decl=\n\
             \    (ApexLocalVarDecl{\n\
             \      modifier=\n\
             \        (Public{\n\
             \          loc=(Location)};\n\
             \      apex_type=\n\
             \        (ApexTypeName{\n\
             \          identifier=\n\
             \            (ApexIdentifier{\n\
             \              name=\n\
             \                \"int\"\n\
             \              loc=\n\
             \                (Location)});\n\
             \          loc=(Location)});\n\
             \      variable_decl=[\n\
             \        (ApexVariableDecl{\n\
             \          id=\n\
             \            (ApexIdentifier{\n\
             \              name=\n\
             \                \"a\"\n\
             \              loc=\n\
             \                (Location)});\n\
             \          loc=\n\
             \            (Location)})];\n\
             \      location=\n\
             \        (Location);});\n\
             \  location=(Location);})" (Buffer.contents buffer) );
       ]
