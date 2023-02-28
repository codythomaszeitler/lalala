open OUnit2
open Lalala.MemberDecl
open Lalala.Location
open Lalala.ApexIdentifier
open Lalala.ApexType

let suite =
  "Member Decl"
  >::: [
         ( "it should be able to pretty print a method decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let methodDecl =
             MethodDeclaration
               ( no_loc,
                 ApexType (no_loc, ApexIdentifier (no_loc, "int")),
                 ApexIdentifier (no_loc, "a"),
                 [] )
           in
           pr_member_decl formatter methodDecl;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(MethodDeclaration{\n\
             \  apex_type=\n\
             \    (ApexType{\n\
             \      identifier=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"int\"\n\
             \          loc=\n\
             \            (Location)});\n\
             \      loc=(Location)})\n\
             \  identifier=\n\
             \    (ApexIdentifier{\n\
             \      name=\n\
             \        \"a\"\n\
             \      loc=\n\
             \        (Location)})\n\
             \  stmts=\n\
             \    []\n\
             \  location=\n\
             \    (Location)})" (Buffer.contents buffer) );
         ( "it should be able to pretty print a field decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let methodDecl =
             FieldDeclaration
               ( no_loc,
                 ApexType (no_loc, ApexIdentifier (no_loc, "int")),
                 [
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "a"));
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "b"));
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "c"));
                 ] )
           in
           pr_member_decl formatter methodDecl;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(FieldDeclaration{\n\
             \  apex_type=\n\
             \    (ApexType{\n\
             \      identifier=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"int\"\n\
             \          loc=\n\
             \            (Location)});\n\
             \      loc=(Location)})\n\
             \  decls=[\n\
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
             \        (Location)})]\n\
             \  location=\n\
             \    (Location)})" (Buffer.contents buffer) );
       ]
