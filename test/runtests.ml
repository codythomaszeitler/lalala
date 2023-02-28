open OUnit2
(* open Lalala.Apexlexer
   open Lalala.Apexparser
   open Lexing
   open Lalala.Ast
   open Lalala.Formatter *)

let suite =
  "test suite for apex lexer"
  >::: [
         (* ( "parse apex class definition empty ast" >:: fun _ ->
              let buffer = from_string "public class TestClass {}" in
              let ast = compilationUnit read_token buffer in
              assert_equal
                (TypeDecl
                   ([ Public ], ClassDeclaration (Identifier "TestClass", [])))
                ast );
            ( "parse apex class definition empty ast with different name"
            >:: fun _ ->
              let buffer = from_string "public class AnotherTestClass {}" in
              let ast = compilationUnit read_token buffer in
              assert_equal
                (TypeDecl
                   ( [ Public ],
                     ClassDeclaration (Identifier "AnotherTestClass", []) ))
                ast );
            ( "parse apex class definition with variable declaration in it"
            >:: fun _ ->
              let buffer =
                from_string
                  "public class AnotherTestClass { public int a; public int \
                   getA() {return a;} }"
              in
              let ast = compilationUnit read_token buffer in
              assert_equal
                (TypeDecl
                   ( [ Public ],
                     ClassDeclaration
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
                         ] ) ))
                ast );
            ( "parse apex class definition with an annotation at the top"
            >:: fun _ ->
              let buffer = from_string "@IsTest public class AppTest {}" in
              let ast = compilationUnit read_token buffer in
              assert_equal
                ~printer:(fun x -> CompilationUnit.to_string x)
                (TypeDecl
                   ( [ Annotation (Identifier "IsTest"); Public ],
                     ClassDeclaration (Identifier "AppTest", []) ))
                ast );
            ( "should be able to create string with num of tabs being 2"
            >:: fun _ ->
              let num_tabs = 2 in
              assert_equal ~printer:(fun x -> x) "    " (tabs num_tabs) );
            ( "should be able to create no space with tabs being 0" >:: fun _ ->
              assert_equal ~printer:(fun x -> x) "" (tabs 0) );
            ( "tostring for a type decl that is a class declaration with no \
               statement"
            >:: fun _ ->
              let typeDecl =
                TypeDecl
                  ( [ Annotation (Identifier "IsTest"); Public ],
                    ClassDeclaration (Identifier "AnotherTestClass", []) )
              in
              let to_string_type_decl = CompilationUnit.to_string typeDecl in
              assert_equal
                ~printer:(fun x -> x)
                "(TypeDecl(\n\
                \  [\n\
                \    (Annotation\n\
                \      (Identifier\n\
                \        (\"IsTest\")));\n\
                \    (Public)\n\
                \  ],\n\
                \  (ClassDeclaration\n\
                \    (Identifier\n\
                \      (\"AnotherTestClass\")),\n\
                \    [])))" to_string_type_decl );
            ClassDeclTest.suite; *)
         StmtTest.suite;
         ApexTypeTest.suite;
         ExprTest.suite;
         LocationTest.suite;
         OperatorTest.suite;
         ApexLiteralTest.suite;
         ApexModifierTest.suite;
         ApexMemberDeclTest.suite;
         ApexIdentifierTest.suite;
         ApexVariableDeclTest.suite;
         ApexLocalVarDeclTest.suite;
         ClassBodyDeclTest.suite;
       ]

let _ = run_test_tt_main suite
