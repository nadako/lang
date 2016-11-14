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
    TypeIsNotCallable(t:Type);
    CouldntInferArgumentType(argName:String);
    CouldntInferReturnType;
    InsufficientTupleElements(remainingTypes:Array<Type>);
    TooManyTupleElements;
    FieldNotFound(type:Type, name:String);
    Immutable;
}

class Typer {
    var localsStack:GenericStack<Map<String,TVar>>;
    var tVoid:Type;
    var tString:Type;
    var tInt:Type;
    var tBool:Type;

    public function new() {
        localsStack = new GenericStack();

        tVoid = typeType(TPath(new TypePath([], "Void")));
        tString = typeType(TPath(new TypePath([], "String")));
        tInt = typeType(TPath(new TypePath([], "Int")));
        tBool = typeType(TPath(new TypePath([], "Bool")));

        var locals = pushLocals();
        locals["trace"] = new TVar("trace", TFun([new TFunctionArg("str", tString)], tVoid));
        locals["monoFun"] = new TVar("monoFun", mkMono());
    }

    public function typeDecl(decl:Decl):TDecl {
        return switch (decl.kind) {
            case DFunction(fun):
                TDFunction(typeFunctionDecl(fun, decl.pos));
            case DClass(cls):
                typeClass(cls, decl.name, decl.pos);
        }
    }

    function typeType(t:Null<SyntaxType>):Type {
        if (t == null)
            return mkMono();
        return switch (t) {
            case TPath(path):
                var decl = loadType(path.module, path.name);
                switch (decl) {
                    case TDClass(cl): TInst(cl);
                    case TDFunction(_): throw false;
                }

            case TTuple(types):
                TTuple(types.map(typeType));

            case TConst(t):
                TConst(typeType(t));

            case TFunction(args, ret):
                TFun([for (a in args) new TFunctionArg(a.name, typeType(a.type))], typeType(ret));
        }
    }

    function typeFunctionDecl(fun:FunctionDecl, pos:Position):TFunctionDecl {
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
            for (arg in decl.args) {
                if (isMono(arg.type))
                    throw new TyperError(CouldntInferArgumentType(arg.name), decl.pos);
            }
            if (isMono(decl.ret)) {
                //throw new TyperError(CouldntInferReturnType, decl.pos);
                unify(decl.ret, tVoid); // TODO: check return expressions
            }
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

    function typeClass(classDecl:ClassDecl, name:String, pos:Position):TDecl {
        var cls = new TClassDecl();
        var decl = TDClass(cls);
        typeCache[name] = decl;

        cls.module = [];
        cls.name = name;
        cls.pos = pos;
        var fields = [];
        for (field in classDecl.fields) {
            switch (field.kind) {
                case FVar(type, expr):
                    fields.push(new TClassField(field.name, typeType(type), field.pos));
                case FFun(_):
                    throw "TODO";
            }
        }
        cls.fields = fields;
        return decl;
    }

    function typeExpr(e:Expr):TExpr {
        return switch (e.kind) {
            case EParens(e):
                typeExpr(e);

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

            case EBinop(OpAssign, left, right):
                var left = typeExpr(left);
                var right = typeExpr(right);
                unifyThrow(right.type, left.type, e.pos);
                switch (left.kind) {
                    case TField({type: TConst(_)}, _):
                        throw new TyperError(Immutable, e.pos);
                    case _:
                }
                new TExpr(TBinop(OpAssign, left, right), left.type, e.pos);

            case ETuple(exprs):
                typeTuple(exprs, e.pos);

            case EField(eobj, name):
                var eobj = typeExpr(eobj);
                inline function getField(cls) {
                    var field = cls.getField(name);
                    if (field == null)
                        throw new TyperError(FieldNotFound(eobj.type, name), e.pos);
                    return field;
                }
                switch (follow(eobj.type)) {
                    case TInst(cls):
                        var field = getField(cls);
                        return new TExpr(TField(eobj, FClassField(cls, field)), field.type, e.pos);
                    case TConst(TInst(cls)):
                        var field = getField(cls);
                        return new TExpr(TField(eobj, FClassField(cls, field)), TConst(field.type), e.pos);
                    default:
                        throw "todo";
                }

            case EIf(econd, ethen, eelse):
                typeIf(econd, ethen, eelse, e.pos);

            case EWhile(econd, ebody):
                typeWhile(econd, ebody, e.pos);

            case EBreak:
                new TExpr(TBreak, mkMono(), e.pos);

            case EContinue:
                new TExpr(TContinue, mkMono(), e.pos);

            case EReturn(re):
                var re = if (re == null) null else typeExpr(re);
                new TExpr(TReturn(re), mkMono(), e.pos);

            case EArrowFunction(args, ret, expr):
                typeFunctionExpr(args, ret, expr, e.pos);

            case ENew(path):
                var tdecl = loadType(path.module, path.name);
                switch (tdecl) {
                    case TDClass(cl): new TExpr(TNew(cl), TInst(cl), e.pos);
                    case TDFunction(_): throw false;
                }
        }
    }

    function typeFunctionExpr(args:Array<FunctionArg>, ret:Null<SyntaxType>, expr:Expr, pos:Position):TExpr {
        var locals = pushLocals();

        var typedArgs = [];
        for (arg in args) {
            var type = typeType(arg.type);
            typedArgs.push(new TFunctionArg(arg.name, type));
            locals[arg.name] = new TVar(arg.name, type);
        }

        var expr = typeExpr(expr);

        popLocals();

        var ret = typeType(ret);
        unifyThrow(expr.type, ret, pos);

        return new TExpr(TFunction(typedArgs, ret, expr), TFun(typedArgs, ret), pos);
    }

    function typeIf(econd:Expr, ethen:Expr, eelse:Null<Expr>, pos:Position):TExpr {
        var econd = typeExpr(econd);
        unifyThrow(econd.type, tBool, econd.pos);
        var ethen = typeExpr(ethen);
        var type = ethen.type;
        var eelse =
            if (eelse != null) {
                var e = typeExpr(eelse);
                unifyThrow(e.type, type, e.pos);
                e;
            } else {
                null;
            };
        return new TExpr(TIf(econd, ethen, eelse), type, pos);
    }

    function typeWhile(econd:Expr, ebody:Expr, pos:Position):TExpr {
        var econd = typeExpr(econd);
        unifyThrow(econd.type, tBool, econd.pos);
        var ebody = typeExpr(ebody);
        return new TExpr(TWhile(econd, ebody), tVoid, pos);
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
                throw new TyperError(TypeIsNotCallable(other), pos);
        }
        return new TExpr(TCall(eobj, typedArgs), returnType, pos);
    }

    function typeTuple(exprs:Array<Expr>, pos:Position):TExpr {
        var types = [];
        var typedExprs = [];
        for (e in exprs) {
            var e = typeExpr(e);
            typedExprs.push(e);
            types.push(e.type);
        }
        return new TExpr(TTuple(typedExprs), TTuple(types), pos);
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

    static function isMono(t:Type):Bool {
        return switch (t) {
            case TMono(m):
                if (m.type == null)
                    true;
                else
                    isMono(m.type);
            default:
                false;
        }
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
            case [TConst(a), TConst(b)]:
                unify(a, b);
            case [a, TConst(b)]:
                unify(a, b); // allow assigning from non-const to const
            // TODO: allow copy-assigning from const to non-const (e.g. basic types like Int)
            case [TInst(ca), TInst(cb)]:
                ca == cb;
            case [TTuple(ta), TTuple(tb)] if (ta.length == tb.length):
                var result = true;
                for (i in 0...ta.length) {
                    if (!unify(ta[i], tb[i])) {
                        result = false;
                        break;
                    }
                }
                result;
            case [TFun(aargs, aret), TFun(bargs, bret)] if (aargs.length == bargs.length):
                if (!unify(aret, bret)) { // unify a with b - covariance
                    false;
                } else {
                    for (i in 0...aargs.length) {
                        if (!unify(bargs[i].type, aargs[i].type)) // unify b with a - contravariance
                            return false;
                    }
                    true;
                }
            default:
                false;
        }
    }
}
