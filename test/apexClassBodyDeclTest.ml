open OUnit2
open Lalala.ApexClassBodyDecl
open Lalala.Location
open Lalala.ApexModifier
open Lalala.ApexMemberDecl

let suite =
  "ClassBodyDecl"
  >::: [
         ( "it should be able to pretty print a class body decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let ast =
             ApexClassBodyDeclaration
               ( no_loc,
                 Public no_loc,
                 ApexMethodDeclaration
                   ( no_loc,
                     ApexTypeName (no_loc, ApexIdentifier (no_loc, "int")),
                     ApexIdentifier (no_loc, "a"),
                     [] ) )
           in
           pr_class_body_decl formatter ast;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexClassBodyDeclaration{\n\
             \  modifier=\n\
             \    (Public{\n\
             \      loc=(Location)};\n\
             \   member_decl=\n\
             \    (ApexMethodDeclaration{\n\
             \      apex_type=\n\
             \        (ApexTypeName{\n\
             \          identifier=\n\
             \            (ApexIdentifier{\n\
             \              name=\n\
             \                \"int\"\n\
             \              loc=\n\
             \                (Location)});\n\
             \          loc=(Location)})\n\
             \      identifier=\n\
             \        (ApexIdentifier{\n\
             \          name=\n\
             \            \"a\"\n\
             \          loc=\n\
             \            (Location)})\n\
             \      stmts=\n\
             \        []\n\
             \      location=\n\
             \        (Location)});\n\
             \  location=\n\
             \    (Location)" (Buffer.contents buffer) );
       ]
