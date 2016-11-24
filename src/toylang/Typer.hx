package toylang;

import haxe.ds.GenericStack;
import toylang.BasicBlock;
import toylang.Syntax;
import toylang.SyntaxEdge;
import toylang.Type;
import toylang.Matcher;

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
    TypeNotFound(module:Array<String>, name:String);
    FieldNotFound(type:Type, name:String);
    Immutable;
    InvalidAssignment;
    ComplexVariableBindingMustHaveInitialValue;
    MatchNotExhaustive;
}

class LoopContext {
    public var head:BasicBlock;
    public var next:BasicBlock;

    public function new(head:BasicBlock, next:BasicBlock) {
        this.head = head;
        this.next = next;
    }
}

class Typer {
    var localsStack:GenericStack<Map<String,TVar>>;
    var thisStack:GenericStack<Type>;
    var loopStack:GenericStack<LoopContext>;
    var tmpCount:Int;
    var bbUnreachable:BasicBlock;

    public var tVoid:Type;
    public var tString:Type;
    public var tInt:Type;
    public var tBool:Type;

    public function new() {
        localsStack = new GenericStack();
        thisStack = new GenericStack();

        for (name in ["Void", "String", "Int", "Bool"]) {
            var decl = new ClassDecl();
            decl.fields = [];
            typeClass(decl, name, Position.nullPos);
        }

        tVoid = typeType(TPath(new TypePath([], "Void")), Position.nullPos);
        tString = typeType(TPath(new TypePath([], "String")), Position.nullPos);
        tInt = typeType(TPath(new TypePath([], "Int")), Position.nullPos);
        tBool = typeType(TPath(new TypePath([], "Bool")), Position.nullPos);

        var locals = pushLocals();
        locals["trace"] = new TVar("trace", TFun([new TFunctionArg("str", tString)], tVoid));
        locals["monoFun"] = new TVar("monoFun", mkMono());
    }

    public function typeSyntaxDecl(decl:SyntaxDecl):TDecl {
        return switch (decl.kind) {
            case DFunction(fun):
                TDFunction(typeFunctionDecl(fun, decl.pos));
            case DClass(cls):
                typeClass(cls, decl.name, decl.pos);
        }
    }

    function typeType(t:Null<SyntaxType>, pos:Position):Type {
        if (t == null)
            return mkMono();
        return switch (t) {
            case TPath(path):
                var decl = loadType(path.module, path.name, pos);
                switch (decl) {
                    case TDClass(cl): TInst(cl);
                    case TDFunction(_): throw false;
                }

            case TTuple(types):
                TTuple([for (t in types) typeType(t, pos)]);

            case TConst(t):
                TConst(typeType(t, pos));

            case TFunction(args, ret):
                TFun([for (a in args) new TFunctionArg(a.name, typeType(a.type, pos))], typeType(ret, pos));
        }
    }

    function typeFunctionDecl(fun:FunctionDecl, pos:Position):TFunctionDecl {
        var decl = new TFunctionDecl();
        decl.module = [];
        decl.name = fun.name;
        decl.pos = pos;
        decl.ret = typeType(fun.ret, pos);
        decl.args = [];

        if (fun.expr != null) {
            var locals = pushLocals();
            for (arg in fun.args) {
                var type = typeType(arg.type, pos);
                decl.args.push(new TFunctionArg(arg.name, type));
                locals[arg.name] = new TVar(arg.name, type);
            }
            tmpCount = 0;
            loopStack = new GenericStack();
            bbUnreachable = new UnreachableBlock();

            var bbRoot = new BasicBlock();
            block(bbRoot, fun.expr);
            decl.cfg = bbRoot;

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
                var type = typeType(arg.type, pos);
                decl.args.push(new TFunctionArg(arg.name, type));
            }
        }

        return decl;
    }

    function block(bb:BasicBlock, e:Expr):BasicBlock {
        var exprs = switch (e.kind) {
            case EBlock(exprs): exprs;
            default: [e];
        }
        for (e in exprs)
            bb = blockElement(bb, e);
        return bb;
    }

    function declareVar(bb:BasicBlock, name:String, type:Type, pos:Position, ?einit:TExpr):TVar {
        var v = new TVar(name, type);
        localsStack.first()[name] = v;
        bb.addElement(new TExpr(TVar(v, einit), tVoid, pos));
        return v;
    }

    function assignVar(bb:BasicBlock, v:TVar, e:TExpr, pos:Position) {
        unifyThrow(e.type, v.type, pos);
        bb.addElement(new TExpr(TAssign(ATVar(v), e), v.type, pos));
    }

    public function typeLiteral(l:Literal, pos:Position):TExpr {
        return switch (l) {
            case LString(s):
                new TExpr(TLiteral(LString(s)), tString, pos);
            case LInt(i):
                var i = Std.parseInt(i);
                if (i == null)
                    throw 'Invalid integer $i';
                new TExpr(TLiteral(LInt(i)), tInt, pos);
            case LBool(b):
                new TExpr(TLiteral(LBool(b)), tBool, pos);
        }
    }

    function value(bb:BasicBlock, e:Expr):{bb:BasicBlock, expr:TExpr} {
        return switch (e.kind) {
            case EParens(e):
                value(bb, e);

            case ELiteral(l):
                {bb: bb, expr: typeLiteral(l, e.pos)};

            case EIdent(ident):
                {bb: bb, expr: resolveIdent(ident, e.pos)};

            case ETuple(exprs):
                var types = [];
                var typedExprs = [];
                for (e in exprs) {
                    var r = value(bb, e);
                    bb = r.bb;
                    typedExprs.push(r.expr);
                    types.push(r.expr.type);
                }
                {bb: bb, expr: new TExpr(TTuple(typedExprs), TTuple(types), e.pos)};

            case EField(eobj, name):
                var r = value(bb, eobj);
                {bb: r.bb, expr: typeField(r.expr, name, e.pos)};

            case EVar(_, _, _):
                throw "var declaration is not allowed in a value place";

            case EWhile(_, _):
                throw "while loop is not allowed in a value place";

            case EBlock([]):
                throw "empty blocks are not allowed in a value place";

            case EBlock(el):
                var last = el[el.length - 1];
                pushLocals();
                for (i in 0...el.length - 1)
                    bb = blockElement(bb, el[i]);
                var r = value(bb, last);
                popLocals();
                r;

            case ENew(path):
                var tdecl = loadType(path.module, path.name, e.pos);
                var expr = switch (tdecl) {
                    case TDClass(cl): new TExpr(TNew(cl), TInst(cl), e.pos);
                    case TDFunction(_): throw false;
                }
                {bb: bb, expr: expr};

            case ECall(eobj, eargs):
                call(bb, eobj, eargs, e.pos);

            case EUnop(op, expr, postfix):
                var r = value(bb, expr);
                var type = switch [op, postfix] {
                    case [OpNeg, false]:
                        unifyThrow(r.expr.type, tInt, e.pos);
                        tInt;
                    case [OpNot, false]:
                        unifyThrow(r.expr.type, tBool, e.pos);
                        tBool;
                    case _: throw 'Unsupported operator $op (postfix=$postfix)';
                }
                {bb: r.bb, expr: new TExpr(TUnop(op, r.expr, postfix), type, e.pos)};

            case EBinop(OpBoolOr, left, right):
                var ethen = right;
                var econd = new Expr(EUnop(OpNot, left, false), e.pos);
                var eelse = new Expr(EIdent("true"), e.pos);
                value(bb, new Expr(EIf(econd, ethen, eelse), e.pos));

            case EBinop(OpBoolAnd, left, right):
                var ethen = right;
                var econd = left;
                var eelse = new Expr(EIdent("false"), e.pos);
                value(bb, new Expr(EIf(econd, ethen, eelse), e.pos));

            case EBinop(op, left, right):
                var left = {
                    var r = value(bb, left);
                    bb = r.bb;
                    r.expr;
                };
                var right = {
                    var r = value(bb, right);
                    bb = r.bb;
                    r.expr;
                };
                unifyThrow(right.type, left.type, e.pos);
                var type = switch (op) {
                    case OpAdd | OpSub | OpMul | OpDiv:
                        left.type;
                    case OpEq | OpNotEq | OpLt | OpLte | OpGt | OpGte:
                        tBool;
                    case _:
                        throw "&& and || are handled above";
                }
                {bb: bb, expr: new TExpr(TBinop(op, left, right), type, e.pos)};

            case EAssign(left, right):
                var left = {
                    var r = value(bb, left);
                    bb = r.bb;
                    r.expr;
                };
                var right = {
                    var r = value(bb, right);
                    bb = r.bb;
                    r.expr;
                };
                var kind =
                    switch (left.kind) {
                        case TLocal(v):
                            TAssign(ATVar(v), right);
                        case TVarField(obj, f):
                            if (obj.type.match(TConst(_)))
                                throw new TyperError(Immutable, e.pos);
                            TAssign(ATField(obj, f), right);
                        default:
                            throw new TyperError(InvalidAssignment, e.pos);
                    }
                unifyThrow(right.type, left.type, e.pos);
                {bb: bb, expr: new TExpr(kind, left.type, e.pos)};

            case EIf(econd, ethen, eelse):
                if (eelse == null)
                    throw "if in a value place must have else branch";

                var type = mkMono();
                var tmpVar = declareVar(bb, "tmpIfElseResult" + (tmpCount++), type, e.pos);

                var r = value(bb, econd);
                r.bb.addElement(r.expr);

                var bbNext = new BasicBlock();
                var bbThen = new BasicBlock();
                var bbElse = new BasicBlock();
                {
                    r.bb.addEdge(bbThen, "then");
                    var r = value(bbThen, ethen);
                    unifyThrow(r.expr.type, type, e.pos);
                    assignVar(r.bb, tmpVar, r.expr, e.pos);
                    r.bb.addEdge(bbNext, "next");
                }
                {
                    r.bb.addEdge(bbElse, "else");
                    var r = value(bbElse, eelse);
                    unifyThrow(r.expr.type, type, e.pos);
                    assignVar(r.bb, tmpVar, r.expr, e.pos);
                    r.bb.addEdge(bbNext, "next");
                }

                r.bb.syntaxEdge = SEBranch(bbThen, bbElse, bbNext);

                {bb: bbNext, expr: new TExpr(TLocal(tmpVar), tmpVar.type, e.pos)};

            case EBreak | EContinue | EReturn(_):
                var bb = blockElement(bb, e);
                {bb: bb, expr: new TExpr(TFakeValue, mkMono(), e.pos)};

            case ESwitch(evalue, cases):
                var tmpResultVar = declareVar(bb, "tmpSwitchResult" + (tmpCount++), mkMono(), e.pos);

                var tmpVar = declareVar(bb, "tmpSwitchSubject" + (tmpCount++), mkMono(), e.pos);
                var r = value(bb, evalue);
                assignVar(r.bb, tmpVar, r.expr, e.pos);

                var matcher = new Matcher(this);
                var dt = matcher.match(new TExpr(TLocal(tmpVar), tmpVar.type, e.pos), cases);
                Matcher.makeDTGraph(dt);

                var bbNext = pattern(r.bb, dt, e.pos, function(bb, expr) {
                    var r = value(bb, expr);
                    assignVar(r.bb, tmpResultVar, r.expr, expr.pos);
                    return r.bb;
                });

                {bb: bbNext, expr: new TExpr(TLocal(tmpResultVar), tmpResultVar.type, e.pos)}

            case EArrowFunction(args, ret, expr):
                var locals = pushLocals();
                var typedArgs = [];
                for (arg in args) {
                    var type = typeType(arg.type, e.pos);
                    typedArgs.push(new TFunctionArg(arg.name, type));
                    locals[arg.name] = new TVar(arg.name, type);
                }

                var oldLoopStack = loopStack;
                loopStack = new GenericStack();

                var bbRoot = new BasicBlock();
                block(bbRoot, new Expr(EReturn(expr), expr.pos));

                popLocals();
                loopStack = oldLoopStack;

                var ret = typeType(ret, e.pos);

                {bb: bb, expr: new TExpr(TFunction(typedArgs, ret, bbRoot), TFun(typedArgs, ret), e.pos)};
        }
    }

    function pattern<T>(bb:BasicBlock, dt:DecisionTree, pos:Position, processLeaf:BasicBlock->Expr->BasicBlock):BasicBlock {
        return switch (dt) {
            case DLeaf(expr):
                processLeaf(bb, expr);

            case DSwitch(subject, cases, def):
                bb.addElement(subject);

                var bbNext = new BasicBlock();

                var cfgCases:Array<SESwitchCase> = [];
                for (c in cases) {
                    var bbCase = new BasicBlock();
                    bb.addEdge(bbCase, 'case ${c.ctor}');

                    var bb = pattern(bbCase, c.dt, pos, processLeaf);
                    bb.addEdge(bbNext, "next");

                    var casePatternExpr = switch (c.ctor) {
                        case CLiteral(l):
                            var t = switch (l) {
                                case LBool(_): tBool;
                                case LInt(_): tInt;
                                case LString(_): tString;
                            }
                            new TExpr(TLiteral(l), t, pos);
                    };
                    cfgCases.push({expr: casePatternExpr, body: bbCase});
                }

                var bbDef = null;
                if (def != null) {
                    bbDef = new BasicBlock();
                    bb.addEdge(bbDef, 'default');

                    var bb = pattern(bbDef, def, pos, processLeaf);
                    bb.addEdge(bbNext, "next");
                }

                bb.syntaxEdge = SESwitch(cfgCases, bbDef, bbNext);

                bbNext;

            case DFail:
                throw new TyperError(MatchNotExhaustive, pos);
        }
    }

    function blockElement(bb:BasicBlock, e:Expr):BasicBlock {
        return switch (e.kind) {
            case EVar(bind, type, einitial):
                var type = typeType(type, e.pos);

                switch (bind) {
                    case VName(name):
                        var v = declareVar(bb, name, type, e.pos);
                        if (einitial != null) {
                            var r = value(bb, einitial);
                            bb = r.bb;
                            unifyThrow(r.expr.type, type, einitial.pos);
                            assignVar(bb, v, r.expr, e.pos);
                        }

                    case VTuple(binds):
                        if (einitial == null)
                            throw new TyperError(ComplexVariableBindingMustHaveInitialValue, e.pos);

                        var tmpVar = declareVar(bb, "tUnpack" + (tmpCount++), type, e.pos);
                        var r = value(bb, einitial);
                        bb = r.bb;
                        assignVar(bb, tmpVar, r.expr, e.pos);

                        function loop(expr:TExpr, binds:Array<VarBinding>) {
                            var i = 0;
                            for (bind in binds) {
                                var elementType = mkMono();
                                var elementExpr = new TExpr(TTupleElement(expr, i), elementType, e.pos);
                                switch (bind) {
                                    case VName("_"):
                                        // skip binding

                                    case VName(name):
                                        declareVar(bb, name, elementType, e.pos, elementExpr);

                                    case VTuple(binds):
                                        var tmpVar = declareVar(bb, "tUnpack" + (tmpCount++), elementType, e.pos, elementExpr);
                                        loop(new TExpr(TLocal(tmpVar), tmpVar.type, e.pos), binds);
                                }
                                i++;
                            }
                        }

                        var elocal = new TExpr(TLocal(tmpVar), tmpVar.type, e.pos);
                        loop(elocal, binds);
                };

                bb;

            case EBlock([]):
                // who needs empty blocks?
                bb;

            case EArrowFunction(_, _, _):
                // block-level arrow functions don't make sense
                bb;

            case EParens(e):
                blockElement(bb, e);

            case EBlock(exprs):
                pushLocals();
                for (e in exprs)
                    bb = blockElement(bb, e);
                popLocals();
                bb;

            case EField(_, _) | ELiteral(_) | EIdent(_) | ETuple(_) | ECall(_, _) | ENew(_) | EUnop(_, _, _), EBinop(_, _, _) | EAssign(_, _):
                var r = value(bb, e);
                r.bb.addElement(r.expr); // some of them (e.g. literals) are not really needed
                r.bb;

            case EIf(econd, ethen, eelse):
                var r = value(bb, econd);
                unifyThrow(r.expr.type, tBool, econd.pos);
                r.bb.addElement(r.expr);

                var bbNext = new BasicBlock();

                var bbThen = new BasicBlock();
                r.bb.addEdge(bbThen, "then");
                var bbThenNext = block(bbThen, ethen);
                bbThenNext.addEdge(bbNext, "next");

                var bbElse;
                if (eelse == null) {
                    bbElse = null;
                    r.bb.addEdge(bbNext, "else");
                } else {
                    bbElse = new BasicBlock();
                    r.bb.addEdge(bbElse, "else");
                    var bbElseNext = block(bbElse, eelse);
                    bbElseNext.addEdge(bbNext, "next");
                }

                r.bb.syntaxEdge = SEBranch(bbThen, bbElse, bbNext);

                bbNext;

            case ESwitch(evalue, cases):
                var tmpVar = declareVar(bb, "tmpSwitchSubject" + (tmpCount++), mkMono(), e.pos);
                var r = value(bb, evalue);
                assignVar(r.bb, tmpVar, r.expr, e.pos);
                var tmpLocal = new TExpr(TLocal(tmpVar), tmpVar.type, e.pos);
                var matcher = new Matcher(this);
                var dt = matcher.match(tmpLocal, cases);
                pattern(r.bb, dt, e.pos, blockElement);

            case EWhile(econd, ebody):
                var bbLoopHead = new BasicBlock();
                var r = value(bbLoopHead, econd);
                unifyThrow(r.expr.type, tBool, econd.pos);
                r.bb.addElement(r.expr);

                var bbNext = new BasicBlock();
                bbLoopHead.addEdge(bbNext, "else");

                var bbLoopBody = new BasicBlock();
                bbLoopHead.addEdge(bbLoopBody, "then");
                loopStack.add(new LoopContext(bbLoopHead, bbNext));
                var bbLoopBodyNext = block(bbLoopBody, ebody);
                loopStack.pop();
                bbLoopBodyNext.addEdge(bbLoopHead, "loop");

                bb.addEdge(bbLoopHead, "next");
                bb.syntaxEdge = SELoop(bbLoopHead, bbLoopBody, bbNext);

                bbNext;

            case EContinue:
                var loopCtx = loopStack.first();
                if (loopCtx == null)
                    throw "continue outside of loop";
                bb.addEdge(loopCtx.head, "continue");
                bbUnreachable;

            case EBreak:
                var loopCtx = loopStack.first();
                if (loopCtx == null)
                    throw "break outside of loop";
                bb.addEdge(loopCtx.next, "break");
                bbUnreachable;

            case EReturn(rvalue):
                if (rvalue == null) {
                    bb.addElement(new TExpr(TReturn(null), tVoid, e.pos));
                } else {
                    var r = value(bb, rvalue);
                    r.bb.addElement(new TExpr(TReturn(r.expr), tVoid, e.pos));
                }
                bbUnreachable;
        }
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
        var fields = cls.fields = [];
        for (field in classDecl.fields) {
            switch (field.kind) {
                case FVar(type, expr):
                    var f = new TClassField(field.name, FVar, typeType(type, pos), field.pos);
                    // if (expr != null)
                    //     f.expr = typeExpr(expr);
                    fields.push(f);
                case FFun(fun):
                    var isConst = field.modifiers.indexOf(FMConst) != -1;
                    var thisType = TInst(cls);
                    if (isConst) thisType = TConst(thisType);
                    thisStack.add(thisType);
                    var tfun = typeFunctionDecl(fun, field.pos);
                    thisStack.pop();
                    fields.push(new TClassField(field.name, FMethod(isConst), TFun(tfun.args, tfun.ret), field.pos));
            }
        }
        return decl;
    }

    function typeField(eobj:TExpr, name:String, pos:Position):TExpr {
        var cls, isConst;
        switch (follow(eobj.type)) {
            case TInst(c):
                cls = c;
                isConst = false;
            case TConst(TInst(c)):
                cls = c;
                isConst = true;
            default:
                throw "todo";
        }
        var field = cls.getField(name);
        if (field == null)
            throw new TyperError(FieldNotFound(eobj.type, name), pos);
        var type = field.type;
        var kind = switch (field.kind) {
            case FVar:
                if (isConst) type = TConst(type);
                TVarField(eobj, FClassField(cls, field));
            case FMethod(_):
                TMethodClosure(eobj, FClassField(cls, field));
        }
        return new TExpr(kind, type, pos);
    }

    function call(bb:BasicBlock, eobj:Expr, eargs:Array<Expr>, pos:Position):{bb:BasicBlock, expr:TExpr} {
        var eobj = {
            var r = value(bb, eobj);
            bb = r.bb;
            r.expr;
        };

        var typedArgs = [];
        for (e in eargs) {
            var tmpVarName = "callArg" + (tmpCount++);
            var r = value(bb, e);
            bb = r.bb;
            var v = declareVar(bb, tmpVarName, r.expr.type, e.pos, r.expr);
            typedArgs.push(new TExpr(TLocal(v), v.type, e.pos));
        }

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
        var kind = switch (eobj.kind) {
            case TMethodClosure(e, f):
                switch (f) {
                    case FClassField(_, {kind: FMethod(false)}):
                        if (follow(e.type).match(TConst(_)))
                            throw new TyperError(Immutable, pos);
                    default:
                }
                TMethodCall(e, f, typedArgs);
            default:
                TCall(eobj, typedArgs);
        }
        return {bb: bb, expr: new TExpr(kind, returnType, pos)};
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
        if (ident == "this") {
            var t = thisStack.first();
            if (t != null) {
                return new TExpr(TThis, t, pos);
            }
        } else {
            var local = findLocal(ident);
            if (local != null) {
                return new TExpr(TLocal(local), local.type, pos);
            } else {
                var t = thisStack.first();
                if (t != null) {
                    return typeField(new TExpr(TThis, t, pos), ident, pos);
                }
            }
        }

        throw new TyperError(UnresolvedIdentifier(ident), pos);
    }

    // hackity, like everything else
    var typeCache = new Map<String,TDecl>();
    function loadType(module:Array<String>, name:String, pos:Position):TDecl {
        var key = module.concat([name]).join(".");
        var type = typeCache[key];
        if (type == null)
            throw new TyperError(TypeNotFound(module, name), pos);
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

    public static function follow(a:Type):Type {
        return switch (a) {
            case TMono(m) if (m.type != null):
                follow(m.type);
            default:
                a;
        }
    }

    public static function unifyThrow(a, b, pos) {
        if (!unify(a, b))
            throw new TyperError(UnificationError(a, b), pos);
    }

    public static function unify(a:Type, b:Type):Bool {
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
