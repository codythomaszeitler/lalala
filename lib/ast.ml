type identifier = Identifier of string
type expr = Primary of identifier
type modifier = Public 
type apexType = ApexType of identifier
type variableDecl = VariableDecl of identifier
type localVarDecl = LocalVarDecl of modifier * apexType * variableDecl list
type statement = LocalVarDeclStmt of localVarDecl | ReturnStmt of expr

type memberDeclaration =
  | FieldDeclaration of apexType * variableDecl list
  | MethodDeclaration of apexType * identifier * statement list

type classBodyDeclaration =
  | ClassBodyDeclaration of modifier * memberDeclaration

type classDeclaration =
  | ClassDeclaration of identifier * classBodyDeclaration list

type compilationUnit = TypeDecl of modifier * classDeclaration
