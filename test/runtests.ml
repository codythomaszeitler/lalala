open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast
open Lalala.Location
open Lalala.ApexModifier
open Lalala.ApexIdentifier
open Lalala.ApexAnnotation
open Lalala.ApexClassBodyDecl
open Lalala.ApexMemberDecl

let suite =
  "test suite for apex lexer"
  >::: [
         ( "parse apex class definition empty ast" >:: fun _ ->
           let buffer = from_string "@TestVisible public class TestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal ~printer:to_string
             (ApexClassDeclaration
                ( no_loc,
                  Some (ApexAnnotation (no_loc, "TestVisible")),
                  [ Public no_loc ],
                  ApexIdentifier (no_loc, "TestClass"),
                  [] ))
             ast );
         ( "parse apex class definition empty ast with different name"
         >:: fun _ ->
           let buffer = from_string "public class AnotherTestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (ApexClassDeclaration
                ( no_loc,
                  None,
                  [ Public no_loc ],
                  ApexIdentifier (no_loc, "AnotherTestClass"),
                  [] ))
             ast );
         ( "parse apex class definition with no modifier" >:: fun _ ->
           let buffer = from_string "class AnotherTestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (ApexClassDeclaration
                ( no_loc,
                  None,
                  [],
                  ApexIdentifier (no_loc, "AnotherTestClass"),
                  [] ))
             ast );
         ( "parse apex class definition with variable declaration in it"
         >:: fun _ ->
           let buffer =
             from_string
               "public class AnotherTestClass { public int a; public int \
                getA() {return a;} }"
           in
           let ast = compilationUnit read_token buffer in
           assert_equal ~printer:to_string
             (ApexClassDeclaration
                ( no_loc,
                  None,
                  [ Public no_loc ],
                  ApexIdentifier (no_loc, "AnotherTestClass"),
                  [
                    ApexClassBodyDeclaration
                      ( no_loc,
                        Public no_loc,
                        ApexFieldDeclaration
                          ( no_loc,
                            ApexType (no_loc, "int"),
                            [
                              ApexVariableDecl
                                (no_loc, ApexIdentifier (no_loc, "a"));
                            ] ) );
                    ApexClassBodyDeclaration
                      ( no_loc,
                        Public no_loc,
                        ApexMethodDeclaration
                          ( no_loc,
                            ApexType (no_loc, "int"),
                            ApexIdentifier (no_loc, "getA"),
                            [
                              ApexReturnStmt
                                ( no_loc,
                                  Id (no_loc, ApexIdentifier (no_loc, "a")) );
                            ] ) );
                  ] ))
             ast );
         StmtTest.suite;
         ApexTypeTest.suite;
         ExprTest.suite;
         LocationTest.suite;
         OperatorTest.suite;
         ApexModifierTest.suite;
         ApexMemberDeclTest.suite;
         ApexIdentifierTest.suite;
         ApexVariableDeclTest.suite;
         ApexLocalVarDeclTest.suite;
         ApexClassBodyDeclTest.suite;
         AstTest.suite;
       ]

let _ = run_test_tt_main suite
