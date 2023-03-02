type javaType = JavaType of string
type javaAnnotation = JavaAnnotation of string
type javaIdentifier = JavaIdentifier of string
type javaModifier = JavaPublic | JavaPrivate 
type javaExpr = JavaMethodCall of javaIdentifier * javaExpr list | JavaIntegerLiteral of int 
type javaStmt = JavaReturnStmt | JavaExprStmt of javaExpr

type javaDecl =
  | JavaMethodDecl of
      javaAnnotation option * javaModifier option * javaType * javaIdentifier * javaStmt list
  | JavaClassDecl of javaAnnotation option * javaModifier option * javaIdentifier * javaDecl list

type java = JavaFile of javaDecl



