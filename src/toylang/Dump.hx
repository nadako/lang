package toylang;

import toylang.Type;

class Dump {
    public static function dumpTypeDecl(d:TDecl):String {
        var b = new StringBuf();
        b.add(d.getName());
        b.add(" ");
        switch (d) {
            case TDFunction(fun):
                b.add(dumpPath(fun.module, fun.name));
                b.add(" ");
                var args = [for (a in fun.args) a.name + " : " + dumpType(a.type)].join(", ");
                b.add('($args) => ');
                b.add(dumpType(fun.ret));
                b.add("\n");
                if (fun.expr != null)
                    b.add(dumpExpr(fun.expr, 1));

            case TDClass(cls):
                b.add(dumpPath(cls.module, cls.name));
        }
        return b.toString();
    }

    static function dumpExpr(e:TExpr, level:Int):String {
        var b = new StringBuf();
        inline function indent(l) b.add(StringTools.lpad("", "\t", l));
        indent(level);
        b.add(e.kind.getName());
        switch (e.kind) {
            case TLocal(v):
                b.add("(");
                b.add(v.name);
                b.add(")");

            case TLiteral(l):
                b.add("(");
                b.add(switch (l) {
                    case LString(s): '"' + Lexer.escapeString(s) + '"';
                    case LInt(i): Std.string(i);
                    case LBool(true): "true";
                    case LBool(false): "false";
                });
                b.add(")");

            default:
        }
        b.add(" : ");
        b.add(dumpType(e.type));
        b.add("\n");
        switch (e.kind) {
            case TBlock(exprs):
                for (e in exprs) {
                    b.add(dumpExpr(e, level + 1));
                    b.add("\n");
                }

            case TTuple(exprs):
                for (e in exprs) {
                    b.add(dumpExpr(e, level + 1));
                    b.add("\n");
                }

            case TCall(e, args):
                b.add(dumpExpr(e, level + 1));
                b.add("\n");
                indent(level + 1);
                b.add("-ARGS-\n");
                for (e in args) {
                    b.add(dumpExpr(e, level + 1));
                    b.add("\n");
                }

            case TLocal(v):
            case TLiteral(_):
            case TBreak:
            case TContinue:
            case TReturn(e):
                if (e != null)
                    b.add(dumpExpr(e, level + 1));

            case TVar(v, e):
                indent(level + 1);
                b.add(v.name);
                b.add(" : ");
                b.add(dumpType(v.type));
                if (e != null) {
                    b.add("\n");
                    indent(level + 1);
                    b.add("=\n");
                    b.add(dumpExpr(e, level + 1));
                }

            case TIf(c, t, e):
                b.add(dumpExpr(c, level + 1));
                b.add("\n");
                indent(level + 1);
                b.add("-THEN-\n");
                b.add(dumpExpr(t, level + 1));
                if (e != null) {
                    b.add("\n");
                    indent(level + 1);
                    b.add("-ELSE-\n");
                    b.add(dumpExpr(e, level + 1));
                }

            case TWhile(cond, body):
                b.add(dumpExpr(cond, level + 1));
                b.add("\n");
                indent(level + 1);
                b.add("-BODY-\n");
                b.add(dumpExpr(body, level + 1));

            case TField(e, f):
                throw "TODO";

            case TFunction(_, _, expr):
                b.add(dumpExpr(expr, level + 1));
        }
        return b.toString();
    }

    static function dumpType(t:Type):String {
        return switch (t) {
            case TTuple(types):
                'TTuple(${types.map(dumpType).join(", ")})';
            case TInst(c):
                'TInst(${dumpPath(c.module, c.name)})';
            case TFun(args,ret):
                var args = [for (a in args) (if (a.name != "") a.name + " : "  else "") + dumpType(a.type)].join(", ");
                'TFun(($args) => ${dumpType(ret)})';
            case TMono(m):
                if (m.type == null)
                    'TMono';
                else
                    dumpType(m.type);
        }
    }

    static inline function dumpPath(module:Array<String>, name:String):String {
        return module.concat([name]).join(".");
    }

    public static function typeToString(t:Type):String {
        return switch (t) {
            case TMono(m):
                if (m.type == null)
                    "<unknown>";
                else
                    typeToString(m.type);
            case TInst(c):
                c.module.concat([c.name]).join(".");
            case TTuple(types):
                var b = new StringBuf();
                b.add("(");
                b.add(types.map(typeToString).join(", "));
                if (types.length == 1)
                    b.add(",");
                b.add(")");
                b.toString();
            case TFun(args, ret):
                var b = new StringBuf();
                b.add("(");
                b.add([for (a in args) typeToString(a.type)].join(", "));
                b.add(") => ");
                b.add(typeToString(ret));
                b.toString();
        }
    }
}
