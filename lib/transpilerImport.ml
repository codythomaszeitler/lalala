open Ast
open JavaImport

let transpile (apex : compilationUnit) : javaImport list =
  if is_test_class apex then [ JavaImport "org.junit.jupiter.api.Test" ] else []
