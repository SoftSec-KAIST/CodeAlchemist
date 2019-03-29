namespace AST

type VarDeclKind =
  | Var
  | Const
  | Let

type PropKind =
  | Get
  | Set
  | Init

type MethodDefKind =
  | Method
  | Constructor
  | Set
  | Get

type UpOp =
  | Inc
  | Dec

type UnOp =
  | Pos | Neg
  | Not | BitNot
  | Delete | Void | TypeOf

type BinOp =
  | Add  | Sub | Mul | Div | Mod | Power
  | Or   | Xor | And
  | LShift | RShift | RShiftZ
  | InstanceOf | In
  | Eq | Neq | AbsEq | AbsNeq | Gt | Ge | Lt | Le

type LogicOp =
  | Or
  | And

type AssignOp =
  | Assign
  | Mul | Div | Mod | Add | Sub | Power
  | LShift | RShift | RShiftZ
  | And | Xor | Or

type Id = string

type Literal =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | Regex of string

type Stmt =
  | Block of StmtList
  | Break of Id option
  | Continue of Id option
  | Debugger
  | DoWhile of Stmt * Expr
  | Empty
  | Expr of Expr * string option
  | For of ForInit option * Expr option * Expr option * Stmt
  | ForIn of ForBind * Expr * Stmt
  | ForOf of ForBind * Expr * Stmt
  | FuncDecl of FuncDecl
  | If of Expr * Stmt * Stmt option
  | Labeled of Id * Stmt
  | Return of Expr option
  | Switch of Expr * Case array
  | Throw of Expr
  | Try of StmtList * Catch option * StmtList option
  | VarDecl of VarDecl
  | While of Expr * Stmt
  | With of Expr * Stmt

and Decl =
  | ClassDecl of ClassDecl
  | VarDecl of VarDecl
  | FuncDecl of FuncDecl

and StmtList = StmtListItem array

and StmtListItem =
  | Stmt of Stmt
  | Decl of Decl

and Expr =
  | This
  | Id of Id
  | Literal of Literal
  | TempLiteral of TempLiteral
  | Array of ArrayElem array
  | Object of Property array
  | Function of FuncExpr
  | ArrowFunction of Id option * Params * ArrowFuncBody * bool * bool
  | Class of Id option * Expr option * MethodDef array
  | TaggedTemp of Expr * TempLiteral
  | Member of MemberExpr
  | Super
  | MetaProp of Id * Id
  | New of Expr * Arg array
  | Call of Callee * Arg array
  | Update of UpOp * Expr * bool
  | Await of Expr
  | Unary of UnOp * Expr
  | Binary of BinOp * Expr * Expr
  | Logic of LogicOp * Expr * Expr
  | Cond of Expr * Expr * Expr
  | Yield of Expr option * bool
  | Assign of AssignOp * AssignLeft * Expr
  | Seq of Expr array

and FuncExpr = Id option * Params * StmtList * bool * bool (* gen, async *)

and MemberExpr = Expr * Expr * bool

and ForInit =
  | Expr of Expr
  | VarDecl of VarDecl

and ForBind =
  | VarDecl of VarDecl
  | Binding of Binding

and FuncDecl = Id * Params * StmtList * bool * bool (* gen, async *)

and Params = Param array

and Case = Expr option * Stmt array

and Catch = Binding option * StmtList

and VarDecl = VarDeclKind * VarDeclr array

and VarDeclr = Binding * Expr option

and ClassDecl = Id * Expr option * MethodDef array

and MethodDef = Expr * FuncExpr * MethodDefKind * bool * bool
  (* computed, static*)

and ArrayElem =
  | Expr of Expr
  | Spread of Expr
  | Empty

and Property = Expr * PropVal * PropKind * bool * bool (* computed, shorthand *)

and PropVal =
  | Expr of Expr
  | BindingPt of BindingPt
  | AssignPt of AssignPt
  | Empty

and Callee =
  | Expr of Expr
  | Import

and Arg =
  | Expr of Expr
  | Spread of Expr

and TempLiteral = string array * Expr array

and ArrowFuncBody =
  | Block of StmtList
  | Expr of Expr

and Param =
  | Id of Id
  | BindingPt of BindingPt
  | AssignPt of AssignPt
  | RestElem of Binding

and BindingPt =
  | ArrayPt of ArrayPtElem array
  | ObjectPt of Property array

and AssignPt = Binding * Expr

and ArrayPtElem =
  | Id of Id
  | BindingPt of BindingPt
  | AssignPt of AssignPt
  | RestElem of Binding
  | MemberExpr of MemberExpr
  | Empty

and Binding =
  | Id of Id
  | BindingPt of BindingPt
  | AssignPt of AssignPt
  | MemberExpr of MemberExpr

and AssignLeft =
  | Expr of Expr
  | Binding of Binding

type Program =
  | Script of StmtList
  | Module of string
  // TODO
