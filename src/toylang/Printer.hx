package toylang;

using StringTools;
import toylang.Syntax;

class Printer {
    var indentString:String;

    public function new(indentString = "  ") {
        this.indentString = indentString;
    }

    public function indent(level:Int):String {
        return StringTools.lpad("", indentString, level * indentString.length);
    }

    public function printDecl(decl:Decl):String {
        return switch (decl.kind) {
            case DFunction(fun):
                printFunction(fun, 0);
            case DClass(cls):
                printClass(decl, cls, 0);
        }
    }

    public function printClass(decl:Decl, cls:ClassDecl, level:Int):String {
        var buf = new StringBuf();
        buf.add("class ");
        buf.add(decl.name);
        buf.add(printTypeParams(cls.params));
        buf.add(" {");
        var fst = true;
        for (field in cls.fields) {
            if (fst) {
                fst = false;
                buf.add("\n");
            }
            buf.add(indent(level + 1));
            for (mod in field.modifiers) {
                buf.add(switch (mod) {
                    case FMConst: "const";
                });
                buf.add(" ");
            }
            switch (field.kind) {
                case FFun(fun):
                    buf.add(printFunction(fun, level + 1));
                case FVar(type, initial):
                    buf.add(printVar(VName(field.name), type, initial, level + 1));
                    buf.add(";");
            }
            buf.add("\n");
        }
        buf.add("}");
        return buf.toString();
    }

    function printTypeParams(params:Array<TypeParamDecl>):String {
        return
            if (params.length == 0) ""
            else "<" + [for (p in params) p.name].join(", ") + ">";
    }

    public function printFunction(fun:FunctionDecl, level:Int):String {
        var buf = new StringBuf();
        buf.add("function ");
        buf.add(fun.name);
        buf.add("(");
        var fst = true;
        for (arg in fun.args) {
            if (fst)
                fst = false;
            else
                buf.add(", ");
            buf.add(arg.name);
            if (arg.type != null) {
                buf.add(":");
                buf.add(printSyntaxType(arg.type));
            }
        }
        buf.add(")");
        if (fun.ret != null) {
            buf.add(":");
            buf.add(printSyntaxType(fun.ret));
        }
        if (fun.expr == null) {
            buf.add(";");
        } else {
            buf.add(" ");
            buf.add(printExpr(fun.expr, level));
        }
        return buf.toString();
    }

    public function printVar(binding:VarBinding, type:Null<SyntaxType>, initial:Null<Expr>, level:Int):String {
        var buf = new StringBuf();
        buf.add("var ");
        buf.add(printVarBinding(binding));
        if (type != null) {
            buf.add(":");
            buf.add(printSyntaxType(type));
        }
        if (initial != null) {
            buf.add(" = ");
            buf.add(printExpr(initial, level));
        }
        return buf.toString();
    }

    public function printVarBinding(binding:VarBinding):String {
        return switch (binding) {
            case VName(name):
                name;
            case VTuple(binds):
                switch (binds) {
                    case []: throw "tuple var bindings can't be empty";
                    case [bind]: '(${printVarBinding(bind)},)';
                    case _: '(${[for (b in binds) printVarBinding(b)].join(", ")})';
                }
        }
    }

    function printBlockExpr(buf:StringBuf, expr:Expr, level:Int) {
        buf.add(indent(level));
        var e = printExpr(expr, level);
        buf.add(e);
        if (e.fastCodeAt(e.length - 1) != "}".code)
            buf.add(";");
        buf.add("\n");
    }

    public function printExpr(expr:Expr, level:Int):String {
        return switch (expr.kind) {
            case EBlock(exprs):
                var buf = new StringBuf();
                buf.add("{");
                var fst = true;
                for (e in exprs) {
                    if (fst) {
                        fst = false;
                        buf.add("\n");
                    }
                    printBlockExpr(buf, e, level + 1);
                }
                if (!fst)
                    buf.add(indent(level));
                buf.add("}");
                buf.toString();

            case ECall(expr, args):
                var buf = new StringBuf();
                buf.add(printExpr(expr, level));
                buf.add("(");
                var fst = true;
                for (arg in args) {
                    if (fst)
                        fst = false;
                    else
                        buf.add(", ");
                    buf.add(printExpr(arg, level));
                }
                buf.add(")");
                buf.toString();

            case EField(expr, fieldName):
                var buf = new StringBuf();
                buf.add(printExpr(expr, level));
                buf.add(".");
                buf.add(fieldName);
                buf.toString();

            case EIdent(ident):
                ident;

            case EBreak:
                "break";

            case EContinue:
                "continue";

            case EAssign(eleft, eright):
                var buf = new StringBuf();
                // buf.add("(");
                buf.add(printExpr(eleft, level));
                buf.add(" = ");
                buf.add(printExpr(eright, level));
                // buf.add(")");
                buf.toString();

            case EBinop(op, eleft, eright):
                var buf = new StringBuf();
                // buf.add("(");
                buf.add(printExpr(eleft, level));
                buf.add(" ");
                buf.add(printBinop(op));
                buf.add(" ");
                buf.add(printExpr(eright, level));
                // buf.add(")");
                buf.toString();

            case EUnop(op, expr, postfix):
                var s = '(${printExpr(expr, level)})';
                var op = printUnop(op);
                if (postfix) s + op else op + s;

            case EReturn(e):
                if (e == null)
                    "return";
                else
                    "return " + printExpr(e, level);

            case ELiteral(LString(s)):
                var buf = new StringBuf();
                buf.add("\"");
                buf.add(Lexer.escapeString(s));
                buf.add("\"");
                buf.toString();

            case ELiteral(LInt(s)):
                s;

            case EIf(cond, then, els):
                var buf = new StringBuf();
                buf.add("if (");
                buf.add(printExpr(cond, level));
                buf.add(") ");
                buf.add(printExpr(then, level));
                if (els != null) {
                    buf.add(" else ");
                    buf.add(printExpr(els, level));
                }
                buf.toString();

            case EVar(binding, type, initial):
                printVar(binding, type, initial, level);

            case EParens(e):
                '(${printExpr(e, level)})';

            case ETuple(exprs):
                switch (exprs) {
                    case []: "()";
                    case [expr]: '(${printExpr(expr, level)},)';
                    case _: '(${[for (e in exprs) printExpr(e, level)].join(", ")})';
                }

            case EWhile(cond, body):
                var buf = new StringBuf();
                buf.add("while (");
                buf.add(printExpr(cond, level));
                buf.add(") ");
                buf.add(printExpr(body, level));
                buf.toString();

            case EArrowFunction(args, ret, expr):
                var buf = new StringBuf();
                if (args.length == 1 && args[0].type == null && ret == null) {
                    buf.add(args[0].name);
                } else {
                    buf.add("(");
                    buf.add([
                        for (arg in args)
                            arg.name + if (arg.type == null) "" else ":" + printSyntaxType(arg.type)
                    ].join(", "));
                    buf.add(")");
                    if (ret != null) {
                        buf.add(":");
                        buf.add(printSyntaxType(ret));
                    }
                }
                buf.add(" => ");
                buf.add(printExpr(expr, level));
                buf.toString();

            case ENew(path):
                var buf = new StringBuf();
                buf.add("new ");
                if (path == null) {
                    throw "TODO";
                } else {
                    buf.add(printTypePath(path));
                }
                buf.toString();

            case ESwitch(expr, cases):
                var buf = new StringBuf();
                buf.add("switch ");
                buf.add(printExpr(expr, level));
                buf.add(" {\n");
                for (c in cases) {
                    buf.add(indent(level + 1));
                    buf.add("case ");
                    buf.add(printExpr(c.pattern, level + 1));
                    buf.add(":\n");
                    switch (c.expr.kind) {
                        case EBlock(exprs):
                            for (e in exprs)
                                printBlockExpr(buf, e, level + 2);
                        case _:
                            printBlockExpr(buf, c.expr, level + 2);
                    }
                }
                buf.add(indent(level));
                buf.add("}");
                buf.toString();
        }
    }

    public static function printUnop(op:Unop):String {
        return switch (op) {
            case OpNot: "!";
            case OpNeg: "-";
        }
    }

    public static function printBinop(op:Binop):String {
        return switch (op) {
            case OpAdd: "+";
            case OpSub: "-";
            case OpDiv: "/";
            case OpMul: "*";
            case OpEq: "==";
            case OpNotEq: "!=";
            case OpGt: ">";
            case OpGte: ">=";
            case OpLt: "<";
            case OpLte: "<=";
            case OpBoolAnd: "&&";
            case OpBoolOr: "||";
        };
    }


    public function printSyntaxType(type:SyntaxType):String {
        return switch (type) {
            case TConst(type):
                var b = new StringBuf();
                b.add("const(");
                b.add(printSyntaxType(type));
                b.add(")");
                b.toString();

            case TPath(path):
                printTypePath(path);

            case TFunction(args, ret):
                var b = new StringBuf();
                b.add("(");
                b.add([for (arg in args) '${arg.name}:${printSyntaxType(arg.type)}'].join(", "));
                b.add(")=>");
                b.add(printSyntaxType(ret));
                b.toString();

            case TTuple(types):
                switch (types) {
                    case []: "()";
                    case [type]: '(${printSyntaxType(type)},)';
                    case _: '(${types.map(printSyntaxType).join(", ")})';
                }
        }
    }

    inline function printTypePath(path:TypePath):String {
        return path.module.concat([path.name]).join(".");
    }
}
