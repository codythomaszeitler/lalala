open OUnit2
open Lalala.ApexModifier
open Lalala.Location
open Lalala.Java
open Lalala.Ast
open Lalala.ApexIdentifier
open Lalala.ApexAnnotation
open Lalala.TranspilerModifier

let suite =
  "TranspileAccessModifier"
  >::: [
         ( "it should be able to get a normal class access modifier" >:: fun _ ->
           let top_level_apex_class =
             ApexClassDeclaration
               ( no_loc,
                 None,
                 [ Public no_loc; Virtual no_loc; Static no_loc ],
                 ApexIdentifier (no_loc, "NormalClass"),
                 [] )
           in
           let javaAccessModifier =
             get_class_access_modifier top_level_apex_class
           in
           assert_equal javaAccessModifier (Some JavaPublic) );
         ( "it should be able to convert a tests class top level modifier from \
            private to public"
         >:: fun _ ->
           let top_level_apex_class =
             ApexClassDeclaration
               ( no_loc,
                 Some (IsTest no_loc),
                 [ Public no_loc; Virtual no_loc; Static no_loc ],
                 ApexIdentifier (no_loc, "TestClass"),
                 [] )
           in
           let javaAccessModifier =
             get_class_access_modifier top_level_apex_class
           in
           assert_equal javaAccessModifier (Some JavaPublic) );
         ( "it should throw an exception if there are multiple access \
            modifiers found"
         >:: fun _ ->
           let top_level_apex_class =
             ApexClassDeclaration
               ( no_loc,
                 Some (IsTest no_loc),
                 [ Public no_loc; Private no_loc; Static no_loc ],
                 ApexIdentifier (no_loc, "TestClass"),
                 [] )
           in
           let f () = get_class_access_modifier top_level_apex_class in
           assert_raises
             (ModifierTranspilerException
                ( "Too many access modifiers found",
                  [ Public no_loc; Private no_loc ] ))
             f );
       ]
