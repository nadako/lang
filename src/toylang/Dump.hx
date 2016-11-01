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
        b.add(" : ");
        b.add(dumpType(e.type));
        b.add("\n");
        switch (e.kind) {
            case TBlock(exprs):
                for (e in exprs) {
                    b.add(dumpExpr(e, level + 1));
                    b.add("\n");
                }

            case TCall(e, args):
                b.add(dumpExpr(e, level + 1));
                b.add("\n");
                indent(level + 1);
                b.add("(\n");
                for (e in args) {
                    b.add(dumpExpr(e, level + 1));
                    b.add("\n");
                }
                indent(level + 1);
                b.add(")");

            case TLocal(v):
                indent(level + 1);
                b.add(v.name);

            case TLiteral(LString(s)):
                indent(level + 1);
                b.add(s);

            case TLiteral(LInt(i)):
                indent(level + 1);
                b.add(i);

            case TLiteral(LBool(true)):
                indent(level + 1);
                b.add("true");

            case TLiteral(LBool(false)):
                indent(level + 1);
                b.add("false");

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

            case TField(e, f):
                throw "TODO";
        }
        return b.toString();
    }

    static inline function dumpType(t:Type):String {
        return switch (t) {
            case TInst(c):
                'TInst(${dumpPath(c.module, c.name)})';
            case TFun(args,ret):
                var args = [for (a in args) a.name + " : " + dumpType(a.type)].join(", ");
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
}
