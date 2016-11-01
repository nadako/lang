package toylang;

import haxe.ds.GenericStack;
import toylang.Syntax;
import toylang.Type;

class TyperError {
    public var message:TyperErrorMessage;
    public var pos:Position;

    public function new(message, pos) {
        this.message = message;
        this.pos = pos;
    }

    public function toString():String {
        return '$pos: $message';
    }
}

enum TyperErrorMessage {
    UnresolvedIdentifier(ident:String);
    UnificationError(a:Type, b:Type);
    InsufficientArguments(remainingArgs:Array<TFunctionArg>);
    TooManyArguments;
}

class Typer {
    var localsStack:GenericStack<Map<String,TVar>>;
    var tVoid:Type;
    var tString:Type;
    var tInt:Type;
    var tBool:Type;

    public function new() {
        localsStack = new GenericStack();

        tVoid = typeType(TPath([], "Void"));
        tString = typeType(TPath([], "String"));
        tInt = typeType(TPath([], "Int"));
        tBool = typeType(TPath([], "Bool"));

        var locals = pushLocals();
        locals["trace"] = new TVar("trace", TFun([], tVoid));
        locals["monoFun"] = new TVar("monoFun", mkMono());
    }

    public function typeDecl(decl:Decl):TDecl {
        return switch (decl.kind) {
            case DFunction(fun):
                TDFunction(typeFunction(fun, decl.pos));
            case DClass(cls):
                TDClass(typeClass(cls, decl.name, decl.pos));
        }
    }

    function typeType(t:Null<SyntaxType>):Type {
        if (t == null)
            return mkMono();
        return switch (t) {
            case TPath(module, name):
                var decl = loadType(module, name);
                switch (decl) {
                    case TDClass(cl): TInst(cl);
                    case TDFunction(_): throw false;
                }
        }
    }

    function typeFunction(fun:FunctionDecl, pos:Position):TFunctionDecl {
        var decl = new TFunctionDecl();
        decl.module = [];
        decl.name = fun.name;
        decl.pos = pos;
        decl.ret = typeType(fun.ret);
        decl.args = [];

        if (fun.expr != null) {
            var locals = pushLocals();
            for (arg in fun.args) {
                var type = typeType(arg.type);
                decl.args.push(new TFunctionArg(arg.name, type));
                locals[arg.name] = new TVar(arg.name, type);
            }
            decl.expr = typeExpr(fun.expr);
            popLocals();
        } else {
            for (arg in fun.args) {
                var type = typeType(arg.type);
                decl.args.push(new TFunctionArg(arg.name, type));
            }
        }

        return decl;
    }

    inline function pushLocals():Map<String,TVar> {
        var locals = new Map();
        localsStack.add(locals);
        return locals;
    }

    inline function popLocals() {
        localsStack.pop();
    }

    function typeClass(cls:ClassDecl, name:String, pos:Position):TClassDecl {
        var decl = new TClassDecl();
        decl.module = [];
        decl.name = name;
        decl.pos = pos;
        decl.fields = [for (field in cls.fields) new TClassField(field.name, mkMono(), field.pos)];
        return decl;
    }

    function typeExpr(e:Expr):TExpr {
        return switch (e.kind) {
            case EBlock(exprs):
                var typedExprs = [];
                pushLocals();
                for (e in exprs)
                    typedExprs.push(typeExpr(e));
                popLocals();
                var type =
                    if (typedExprs.length > 0)
                        typedExprs[typedExprs.length - 1].type
                    else
                        tVoid;
                new TExpr(TBlock(typedExprs), type, e.pos);

            case EIdent("false"):
                new TExpr(TLiteral(LBool(false)), tBool, e.pos);

            case EIdent("true"):
                new TExpr(TLiteral(LBool(true)), tBool, e.pos);

            case EIdent(ident):
                resolveIdent(ident, e.pos);

            case ELiteral(LString(s)):
                new TExpr(TLiteral(LString(s)), tString, e.pos);

            case ELiteral(LInt(i)):
                new TExpr(TLiteral(LInt(Std.parseInt(i))), tInt, e.pos);

            case ECall(expr, args):
                typeCall(expr, args, e.pos);

            case EVar(name, type, einitial):
                var type = typeType(type);
                var v = new TVar(name, type);
                var einitial =
                    if (einitial != null) {
                        var e = typeExpr(einitial);
                        unifyThrow(e.type, type, einitial.pos);
                        e;
                    } else {
                        null;
                    }
                localsStack.first()[name] = v;
                new TExpr(TVar(v, einitial), tVoid, e.pos);

            case EField(_, _):
                throw false;
        }
    }

    function typeCall(eobj:Expr, eargs:Array<Expr>, pos:Position):TExpr {
        var eobj = typeExpr(eobj);
        var typedArgs = [for (e in eargs) typeExpr(e)];
        var returnType;
        switch (follow(eobj.type)) {
            case TFun(args, ret):
                if (args.length > typedArgs.length)
                    throw new TyperError(InsufficientArguments(args.slice(typedArgs.length)), pos);
                else if (args.length < typedArgs.length)
                    throw new TyperError(TooManyArguments, pos);

                returnType = ret;

                for (i in 0...args.length) {
                    var arg = args[i];
                    var expr = typedArgs[i];
                    unifyThrow(expr.type, arg.type, expr.pos);
                }

            case TMono(m):
                returnType = mkMono();

                var argName = 'a'.code;
                var args = [];
                for (i in 0...typedArgs.length)
                    args.push(new TFunctionArg(String.fromCharCode(argName + i), typedArgs[i].type));

                m.resolve(TFun(args, returnType));

            case other:
                throw "cannot call non-function type " + other;
        }
        return new TExpr(TCall(eobj, typedArgs), returnType, pos);
    }

    function findLocal(name:String):Null<TVar> {
        for (map in localsStack) {
            var v = map[name];
            if (v != null)
                return v;
        }
        return null;
    }

    function resolveIdent(ident:String, pos:Position):TExpr {
        var local = findLocal(ident);
        if (local != null)
            return new TExpr(TLocal(local), local.type, pos);

        throw new TyperError(UnresolvedIdentifier(ident), pos);
    }

    // hackity, like everything else
    var typeCache = new Map<String,TDecl>();
    function loadType(module:Array<String>, name:String):TDecl {
        var key = module.concat([name]).join(".");
        var type = typeCache[key];
        if (type == null) {
            type = typeCache[key] = TDClass({
                var cl = new TClassDecl();
                cl.module = module;
                cl.name = name;
                cl.pos = Position.nullPos;
                cl.fields = [];
                cl;
            });
        }
        return type;
    }

    inline static function mkMono():Type {
        return TMono(new Monomorph());
    }

    static function follow(a:Type):Type {
        return switch (a) {
            case TMono(m) if (m.type != null):
                follow(m.type);
            default:
                a;
        }
    }

    static function unifyThrow(a, b, pos) {
        if (!unify(a, b))
            throw new TyperError(UnificationError(a, b), pos);
    }

    static function unify(a:Type, b:Type):Bool {
        if (a == b)
            return true;
        return switch [a, b] {
            case [TMono(ma), _]:
                if (ma.type == null) {
                    ma.resolve(b);
                    true;
                } else {
                    unify(ma.type, b);
                }
            case [_, TMono(mb)]:
                if (mb.type == null) {
                    mb.resolve(a);
                    true;
                } else {
                    unify(a, mb.type);
                }
            case [TInst(ca), TInst(cb)]:
                ca == cb;
            default:
                false;
        }
    }
}
