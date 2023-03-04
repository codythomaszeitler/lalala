open OUnit2
open Lalala.ApexDecl
open Lalala.Location
open Lalala.ApexIdentifier
open Lalala.ApexType
open Lalala.ApexAnnotation

let suite =
  "ApexDecl"
  >::: [
         ( "it should be able to pretty print a method decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let methodDecl =
             ApexMethodDeclaration
               ( no_loc,
                 None,
                 [],
                 ApexType (no_loc, "int"),
                 ApexIdentifier (no_loc, "a"),
                 [] )
           in
           pr_member_decl formatter methodDecl;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexMethodDeclaration{\n\
             \  annotation=\n\
             \    \n\
             \  modifiers=\n\
             \    \n\
             \  apex_type=\n\
             \    (ApexType{\n\
             \      identifier=\n\
             \        int;\n\
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
             ApexFieldDeclaration
               ( no_loc,
                 None,
                 [],
                 ApexType (no_loc, "int"),
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
             "(ApexFieldDeclaration{\n\
             \  annotation=\n\
             \    \n\
             \  modifiers=\n\
             \    \n\
             \  apex_type=\n\
             \    (ApexType{\n\
             \      identifier=\n\
             \        int;\n\
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
         ( "it should be able to tell if apex-decl is a test method when it is \
            a test method"
         >:: fun _ ->
           let test_method =
             ApexMethodDeclaration
               ( no_loc,
                 Some (ApexAnnotation (no_loc, "TestMethod")),
                 [],
                 ApexType (no_loc, "void"),
                 ApexIdentifier (no_loc, "fooTest"),
                 [] )
           in
           assert_equal true (is_test_method test_method) );
         ( "it should be able to tell if apex-decl is a test method when it is \
            NOT a test method"
         >:: fun _ ->
           let test_method =
             ApexMethodDeclaration
               ( no_loc,
                 None,
                 [],
                 ApexType (no_loc, "void"),
                 ApexIdentifier (no_loc, "fooTest"),
                 [] )
           in
           assert_equal false (is_test_method test_method) );
         ( "it should be able to tell if there are any test methods within a \
            list of decls"
         >:: fun _ ->
           let decls =
             [
               ApexMethodDeclaration
                 ( no_loc,
                   None,
                   [],
                   ApexType (no_loc, "void"),
                   ApexIdentifier (no_loc, "fooUtil"),
                   [] );
               ApexMethodDeclaration
                 ( no_loc,
                   Some (ApexAnnotation (no_loc, "TestMethod")),
                   [],
                   ApexType (no_loc, "void"),
                   ApexIdentifier (no_loc, "fooTest"),
                   [] );
             ]
           in
           assert_equal true (has_test_method decls) );
         ( "it should be able to tell if there no test methods within a list \
            of decls"
         >:: fun _ ->
           let decls =
             [
               ApexMethodDeclaration
                 ( no_loc,
                   None,
                   [],
                   ApexType (no_loc, "void"),
                   ApexIdentifier (no_loc, "fooUtil"),
                   [] );
             ]
           in
           assert_equal false (has_test_method decls) );
       ]
