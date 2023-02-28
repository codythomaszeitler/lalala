open OUnit2
open Lalala.ApexLocalVarDecl
open Lalala.ApexModifier
open Lalala.ApexTypeName
open Lalala.ApexVariableDecl
open Lalala.ApexIdentifier
open Lalala.Location

let suite =
  "test suite for local var decl ast"
  >::: [
         ( "it should be able to tostring a local var decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let astNode =
             ApexLocalVarDecl
               ( no_loc,
                 Public no_loc,
                 ApexTypeName (no_loc, ApexIdentifier (no_loc, "int")),
                 [
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "a"));
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "b"));
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "c"));
                 ] )
           in
           pr_local_var_decl formatter astNode;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexLocalVarDecl{\n\
             \  modifier=\n\
             \    (Public{\n\
             \      loc=(Location)};\n\
             \  apex_type=\n\
             \    (ApexTypeName{\n\
             \      identifier=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"int\"\n\
             \          loc=\n\
             \            (Location)});\n\
             \      loc=(Location)});\n\
             \  variable_decl=[\n\
             \    (ApexVariableDecl{\n\
             \      id=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"a\"\n\
             \          loc=\n\
             \            (Location)});\n\
             \      loc=\n\
             \        (Location)})\n\
             \    (ApexVariableDecl{\n\
             \      id=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"b\"\n\
             \          loc=\n\
             \            (Location)});\n\
             \      loc=\n\
             \        (Location)})\n\
             \    (ApexVariableDecl{\n\
             \      id=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"c\"\n\
             \          loc=\n\
             \            (Location)});\n\
             \      loc=\n\
             \        (Location)})];\n\
             \  location=\n\
             \    (Location);})" (Buffer.contents buffer) );
       ]
