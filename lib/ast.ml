type loc = Location
type identifier = Identifier of loc * string

type expr = Primary of loc * identifier

type modifier = Public of loc
type apexType = ApexType of loc * identifier
type variableDecl = VariableDecl of loc * identifier

type localVarDecl =
  | LocalVarDecl of loc * modifier * apexType * variableDecl list

type statement = LocalVarDeclStmt of loc * localVarDecl
  | ReturnStmt of loc * expr

type memberDeclaration =
  | FieldDeclaration of loc * apexType * variableDecl list
  | MethodDeclaration of loc * apexType * identifier * statement list

type classBodyDeclaration =
  | ClassBodyDeclaration of loc * modifier * memberDeclaration

type classDeclaration =
  | ClassDeclaration of loc * identifier * classBodyDeclaration list

type compilationUnit = TypeDecl of loc * modifier * classDeclaration
