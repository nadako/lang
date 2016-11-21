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
    TkEquals;
    TkEqualsEquals;
    TkBangEquals;
    TkAmpAmp;
    TkPipePipe;
    TkLt;
    TkGt;
    TkLte;
    TkGte;
    TkBackslash;
    TkEof;
    TkPlus;
    TkMinus;
    TkAsterisk;
    TkSlash;
    TkBang;
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
    KwdNew;
    KwdThis;
    KwdSwitch;
    KwdCase;
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
    public var modifiers:Array<FieldModifier>;

    public function new(name, kind, pos) {
        this.name = name;
        this.kind = kind;
        this.pos = pos;
        modifiers = [];
    }
}

enum FieldModifier {
    FMConst;
}

enum FieldKind {
    FFun(fun:FunctionDecl);
    FVar(type:Null<SyntaxType>, initial:Null<Expr>);
}

enum SyntaxType {
    TPath(path:TypePath);
    TTuple(types:Array<SyntaxType>);
    TFunction(args:Array<FunctionArg>, returnType:SyntaxType);
    TConst(type:SyntaxType);
}

class TypePath {
    public var module:Array<String>;
    public var name:String;

    public function new(module, name) {
        this.module = module;
        this.name = name;
    }
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
    EVar(binding:VarBinding, type:Null<SyntaxType>, initial:Null<Expr>);
    ETuple(exprs:Array<Expr>);
    EParens(expr:Expr);
    ENew(path:Null<TypePath>);
    EIf(cond:Expr, then:Expr, els:Null<Expr>);
    EArrowFunction(args:Array<FunctionArg>, ret:Null<SyntaxType>, expr:Expr);
    EWhile(cond:Expr, body:Expr);
    EAssign(left:Expr, right:Expr);
    EBinop(op:Binop, left:Expr, right:Expr);
    EUnop(op:Unop, expr:Expr, postfix:Bool);
    EBreak;
    EContinue;
    EReturn(e:Null<Expr>);
    ESwitch(expr:Expr, cases:Array<Case>);
}

enum VarBinding {
    VName(name:String);
    VTuple(binds:Array<VarBinding>);
}

class Case {
    public var pattern:Expr;
    public var expr:Expr;

    public function new(pattern, expr) {
        this.pattern = pattern;
        this.expr = expr;
    }
}