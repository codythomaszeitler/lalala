open Ast
open JavaImport

(* So at this point, you are going to have to go through all declarations within the top level class *)
let transpile (apex : compilationUnit) : javaImport list =
  let annotations =
    match apex with
    | ApexClassDeclaration (_, _, _, _, decls)
      when ApexDecl.has_test_method decls ->
        [ JavaImport "static org.junit.Assert.assertEquals" ]
    | _ -> []
  in
  (if is_test_class apex then [ JavaImport "org.junit.Test" ] else [])
  @ annotations
