package toylang;

enum TDecl {
    TDFunction(f:TFunctionDecl);
    TDClass(c:TClassDecl);
}

class TBaseDecl {
    public var module:Array<String>;
    public var name:String;
    public var pos:Position;
    public function new() {}
}

class TClassDecl extends TBaseDecl {
    public var fields:Array<TClassField>;
}

class TFunctionDecl extends TBaseDecl {
    public var args:Array<TFunctionArg>;
    public var ret:Type;
    public var expr:Null<TExpr>;
}

class TFunctionArg {
    public var name:String;
    public var type:Type;

    public function new(name, type) {
        this.name = name;
        this.type = type;
    }
}

class TClassField {
    public var name:String;
    public var pos:Position;
    public var type:Type;

    public function new(name, type, pos) {
        this.name = name;
        this.type = type;
        this.pos = pos;
    }
}

enum Type {
    TMono(m:Monomorph);
    TInst(c:TClassDecl);
    TFun(args:Array<TFunctionArg>, ret:Type);
    TTuple(types:Array<Type>);
}

class Monomorph {
    public var type(default,null):Null<Type>;
    public function new() {}
    public function resolve(type:Type) {
        if (this.type != null)
            throw "Monomorph already resolved";
        this.type = type;
    }
}

class TExpr {
    public var kind:TExprKind;
    public var type:Type;
    public var pos:Position;

    public function new(kind, type, pos) {
        this.kind = kind;
        this.type = type;
        this.pos = pos;
    }
}

class TVar {
    public var name:String;
    public var type:Type;

    public function new(name, type) {
        this.name = name;
        this.type = type;
    }
}

enum TExprKind {
    TBlock(exprs:Array<TExpr>);
    TTuple(exprs:Array<TExpr>);
    TVar(v:TVar, einitial:Null<TExpr>);
    TLocal(v:TVar);
    TField(e:TExpr, f:FieldAccess);
    TCall(e:TExpr, args:Array<TExpr>);
    TLiteral(l:TLiteral);
    TIf(econd:TExpr, ethen:TExpr, eelse:Null<TExpr>);
    TFunction(args:Array<TFunctionArg>, ret:Type, expr:TExpr);
}

enum TLiteral {
    LString(s:String);
    LInt(i:Int);
    LBool(b:Bool);
}

enum FieldAccess {
    FClassField(c:TClassDecl, f:TClassField);
}
