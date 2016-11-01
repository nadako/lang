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
    TkEqual;
    TkEof;
}

enum Keyword {
    KwdFunction;
    KwdClass;
    KwdVar;
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
    public function new() {}
}

class ClassDecl {
    public var fields:Array<Field>;
    public function new() {}
}

class Field {
    public var name:String;
    public var pos:Position;
    public var kind:FieldKind;
    public function new() {}
}

enum FieldKind {
    FFun(fun:FunctionDecl);
    FVar(type:Null<SyntaxType>, initial:Null<Expr>);
}

enum SyntaxType {
    TPath(module:Array<String>, name:String);
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
}
