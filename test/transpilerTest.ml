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
         ( "it should be able to convert empty class definition into junit test"
         >:: fun _ ->
           let apex =
             ApexClassDeclaration
               ( no_loc,
                 Some (ApexAnnotation (no_loc, "IsTest")),
                 [ Private no_loc ],
                 ApexIdentifier (no_loc, "TestClass"),
                 [
                   ApexMethodDeclaration
                     ( no_loc,
                       Some (ApexAnnotation (no_loc, "TestMethod")),
                       [ Private no_loc ],
                       ApexType (no_loc, "void"),
                       ApexIdentifier (no_loc, "testMethod"),
                       [] );
                 ] )
           in

           let expected =
             JavaFile
               (JavaClassDecl
                  ( None,
                    Some JavaPublic,
                    JavaIdentifier "TestClass",
                    [
                      JavaMethodDecl
                        ( Some (JavaAnnotation "Test"),
                          Some JavaPublic,
                          JavaType "void",
                          JavaIdentifier "testMethod",
                          [] );
                    ] ))
           in
           let transpiled = transpile apex in
           assert_equal ~printer:to_string_java expected transpiled );
       ]
