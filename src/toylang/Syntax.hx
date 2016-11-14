package toylang;

class Token {
    public var kind:TokenKind;
    public var pos:Position;

    public function new(kind, pos) {
        this.kind = kind;
        this.pos = pos;
    }

    public function toString():String {
        return '{ $kind | $pos }';
    }
}

enum TokenKind {
    TkKeyword(keyword:Keyword);
    TkIdent(ident:String);
    TkLiteral(literal:Literal);
    TkParenOpen;
    TkParenClose;
    TkBraceOpen;
    TkBraceClose;
    TkComma;
    TkDot;
    TkColon;
    TkSemicolon;
    TkArrow;
    TkEqual;
    TkLt;
    TkGt;
    TkBackslash;
    TkEof;
}

enum Keyword {
    KwdFunction;
    KwdClass;
    KwdVar;
    KwdConst;
    KwdIf;
    KwdElse;
    KwdWhile;
    KwdBreak;
    KwdContinue;
    KwdReturn;
}

enum Literal {
    LString(s:String);
    LInt(s:String);
}

class Decl {
    public var name:String;
    public var pos:Position;
    public var kind:DeclKind;
    public function new() {}
}

enum DeclKind {
    DFunction(fun:FunctionDecl);
    DClass(cls:ClassDecl);
}

class FunctionDecl {
    public var name:String;
    public var args:Array<FunctionArg>;
    public var ret:Null<SyntaxType>;
    public var expr:Null<Expr>;
    public function new() {}
}

class FunctionArg {
    public var name:String;
    public var type:Null<SyntaxType>;

    public function new(name, type) {
        this.name = name;
        this.type = type;
    }
}

class TypeParamDecl {
    public var name:String;

    public function new(name) {
        this.name = name;
    }
}

class ClassDecl {
    public var fields:Array<Field>;
    public var params:Array<TypeParamDecl>;
    public function new() {}
}

class Field {
    public var name:String;
    public var pos:Position;
    public var kind:FieldKind;

    public function new(name, kind, pos) {
        this.name = name;
        this.kind = kind;
        this.pos = pos;
    }
}

enum FieldKind {
    FFun(fun:FunctionDecl);
    FVar(type:Null<SyntaxType>, initial:Null<Expr>);
}

enum SyntaxType {
    TPath(module:Array<String>, name:String);
    TTuple(types:Array<SyntaxType>);
    TFunction(args:Array<FunctionArg>, returnType:SyntaxType);
    TConst(type:SyntaxType);
}

class Expr {
    public var kind:ExprKind;
    public var pos:Position;
    public function new(kind, pos) {
        this.kind = kind;
        this.pos = pos;
    }
}

enum ExprKind {
    EBlock(exprs:Array<Expr>);
    EIdent(ident:String);
    ELiteral(literal:Literal);
    EField(expr:Expr, field:String);
    ECall(expr:Expr, args:Array<Expr>);
    EVar(name:String, type:Null<SyntaxType>, initial:Null<Expr>);
    ETuple(exprs:Array<Expr>);
    EParens(expr:Expr);
    EIf(cond:Expr, then:Expr, els:Null<Expr>);
    EArrowFunction(args:Array<FunctionArg>, ret:Null<SyntaxType>, expr:Expr);
    EWhile(cond:Expr, body:Expr);
    EBinop(op:Binop, left:Expr, right:Expr);
    EBreak;
    EContinue;
    EReturn(e:Null<Expr>);
}
