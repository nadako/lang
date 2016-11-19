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

    public function getField(name:String):TClassField {
        for (f in fields) {
            if (f.name == name)
                return f;
        }
        return null;
    }
}

class TFunctionDecl extends TBaseDecl {
    public var args:Array<TFunctionArg>;
    public var ret:Type;
    public var cfg:Null<BasicBlock>;
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
    public var kind:TClassFieldKind;
    public var type:Type;
    public var pos:Position;

    public function new(name, kind, type, pos) {
        this.name = name;
        this.kind = kind;
        this.type = type;
        this.pos = pos;
    }
}

enum TClassFieldKind {
    FVar;
    FMethod(isConst:Bool);
}

enum Type {
    TMono(m:Monomorph);
    TInst(c:TClassDecl);
    TFun(args:Array<TFunctionArg>, ret:Type);
    TTuple(types:Array<Type>);
    TConst(t:Type);
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
    TThis;
    TTuple(exprs:Array<TExpr>);
    TVar(v:TVar, einitial:Null<TExpr>);
    TLocal(v:TVar);
    TMethodClosure(e:TExpr, f:FieldAccess);
    TVarField(e:TExpr, f:FieldAccess);
    TCall(e:TExpr, args:Array<TExpr>);
    TMethodCall(eobj:TExpr, f:FieldAccess, args:Array<TExpr>);
    TLiteral(l:TLiteral);
    TReturn(e:Null<TExpr>); // todo: remove this
    TFunction(args:Array<TFunctionArg>, ret:Type, expr:TExpr);
    TAssign(target:AssignTarget, value:TExpr);
    TNew(cls:TClassDecl);
    TBinop(op:Binop, left:TExpr, right:TExpr);
    TUnop(op:Unop, expr:TExpr, postfix:Bool);
    TFakeValue; // probably not a good idea
}

enum AssignTarget {
    ATVar(v:TVar);
    ATField(obj:TExpr, f:FieldAccess);
}

enum TLiteral {
    LString(s:String);
    LInt(i:Int);
    LBool(b:Bool);
}

enum FieldAccess {
    FClassField(c:TClassDecl, f:TClassField);
}
