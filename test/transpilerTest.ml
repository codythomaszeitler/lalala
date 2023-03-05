open OUnit2
open Lalala.Java
open Lalala.Transpiler
open Lalala.Ast
open Lalala.Location
open Lalala.ApexModifier
open Lalala.ApexIdentifier
open Lalala.ApexAnnotation

let suite =
  "Transpiler"
  >::: [
         ( "it should be able to convert class definition with one empty test \
            method into junit test"
         >:: fun _ ->
           let apex =
             ApexClassDeclaration
               ( no_loc,
                 Some (IsTest(no_loc)),
                 [ Private no_loc ],
                 ApexIdentifier (no_loc, "TestClass"),
                 [
                   ApexMethodDeclaration
                     ( no_loc,
                       Some (IsTest(no_loc)),
                       [ Private no_loc ],
                       ApexType (no_loc, "void"),
                       ApexIdentifier (no_loc, "testMethod"),
                       [
                         ApexExprStmt
                           ( no_loc,
                             ApexMethodCall
                               ( no_loc,
                                 ApexIdentifier (no_loc, "System.assertEquals"),
                                 [
                                   IntegerLiteral (no_loc, 2);
                                   IntegerLiteral (no_loc, 3);
                                 ] ) );
                       ] );
                 ] )
           in
           let expected =
             JavaFile
               ( [
                   JavaImport "org.junit.jupiter.api.Test";
                   JavaImport "org.junit.jupiter.api.Assertions.assertEquals";
                 ],
                 JavaClassDecl
                   ( None,
                     Some JavaPublic,
                     JavaIdentifier "TestClass",
                     [
                       JavaMethodDecl
                         ( Some (JavaAnnotation "Test"),
                           Some JavaPublic,
                           JavaType "void",
                           JavaIdentifier "testMethod",
                           [
                             JavaExprStmt
                               (JavaMethodCall
                                  ( JavaIdentifier "System.assertEquals",
                                    [
                                      JavaIntegerLiteral 2; JavaIntegerLiteral 3;
                                    ] ));
                           ] );
                     ] ) )
           in
           let transpiled = transpile apex in
           assert_equal ~printer:to_string_java expected transpiled );
       ]
