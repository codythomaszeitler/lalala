open OUnit2
open Lalala.Java
open Lalala.JavaPrinter

let suite =
  "JavaPrinter"
  >::: [
         ( "it should be able to print out an empty class declaration in java"
         >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let java =
             JavaFile
               ( [
                   JavaImport "org.junit.jupiter.api.Test";
                   JavaImport "org.junit.jupiter.api.Assertions.assertEquals";
                 ],
                 JavaClassDecl
                   ( None,
                     None,
                     JavaIdentifier "TestClass",
                     [
                       JavaMethodDecl
                         ( Some JavaTest,
                           Some JavaPublic,
                           JavaType "void",
                           JavaIdentifier "methodFoo",
                           [
                             JavaExprStmt
                               (JavaMethodCall
                                  ( JavaIdentifier "System.assertEquals",
                                    [
                                      JavaIntegerLiteral 2; JavaIntegerLiteral 2;
                                    ] ));
                           ] );
                     ] ) )
           in
           print formatter java;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "import org.junit.jupiter.api.Test;\n\
              import org.junit.jupiter.api.Assertions.assertEquals;\n\n\
              class TestClass {\n\
             \  @Test public void methodFoo() { System.assertEquals(2,2);}\n\
              }\n"
             (Buffer.contents buffer) );
       ]
