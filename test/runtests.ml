open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast

let tests =
  "test suite for apex lexer"
  >::: [
         ( "parse apex class definition empty ast" >:: fun _ ->
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
             ~printer:(fun x -> to_string_compilation_unit x)
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
         ( "tostring for the modifiers single arg" >:: fun _ ->
           let modifiers = [ Public ] in
           assert_equal
             ~printer:(fun x -> x)
             "[\n  (Public)\n]"
             (to_string_modifiers modifiers 0) );
         ( "tostring for the modifiers multiple args" >:: fun _ ->
           let to_string_value =
             to_string_modifiers [ Public; Annotation (Identifier "a") ] 0
           in
           assert_equal
             ~printer:(fun x -> x)
             "[\n\
             \  (Public);\n\
             \  (Annotation\n\
             \    (Identifier\n\
             \      (\"a\")))\n\
              ]"
             to_string_value );
         ( "tostring for a type decl that is a class declaration with no \
            statement"
         >:: fun _ ->
           let typeDecl =
             TypeDecl
               ( [ Annotation (Identifier "IsTest"); Public ],
                 ClassDeclaration (Identifier "AnotherTestClass", []) )
           in
           let to_string_type_decl = to_string_compilation_unit typeDecl in
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
         ( "it should be able to make a tostring out of a class decl body"
         >:: fun _ ->
           let classBodyDecl =
             ClassBodyDeclaration
               ( Public,
                 MethodDeclaration
                   ( ApexType (Identifier "int"),
                     Identifier "getA",
                     [ ReturnStmt (Primary (Identifier "a")) ] ) )
           in
           assert_equal
             ~printer:(fun x -> x)
             "(ClassBodyDeclaration\n\
             \  (Public),\n\
             \  (MethodDeclaration\n\
             \    (ApexType\n\
             \      (Identifier\n\
             \        (\"int\"))),\n\
             \    (Identifier\n\
             \      (\"getA\")),\n\
             \    [\n\
             \      (ReturnStmt\n\
             \        (Primary\n\
             \          (Identifier\n\
             \            (\"a\")))))]))"
             (to_string_class_body_decl classBodyDecl 0) );
         ( "it should be able to tostring a return statement" >:: fun _ ->
           let returnStmt = ReturnStmt (Primary (Identifier "a")) in
           assert_equal
             ~printer:(fun x -> x)
             "(ReturnStmt\n  (Primary\n    (Identifier\n      (\"a\")))))"
             (to_string_statement returnStmt 0) );
         ( "it should be able to tostring a method decl" >:: fun _ ->
           let methodDecl =
             MethodDeclaration
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
             (to_string_member_decl methodDecl 0) );
         ( "it should be able to tostring an apex type" >:: fun _ ->
           let apexType = ApexType (Identifier "int") in
           assert_equal
             ~printer:(fun x -> x)
             "(ApexType\n  (Identifier\n    (\"int\")))"
             (to_string_apex_type apexType 0) );
       ]

let _ = run_test_tt_main tests
