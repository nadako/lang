package toylang;

import toylang.Syntax;

class Printer {
    var indentString:String;

    public function new(indentString = "\t") {
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
        buf.add(" {");
        var fst = true;
        for (field in cls.fields) {
            if (fst) {
                fst = false;
                buf.add("\n");
            }
            buf.add(indent(level + 1));
            switch (field.kind) {
                case FFun(fun):
                    buf.add(printFunction(fun, level + 1));
                case FVar(type, initial):
                    buf.add(printVar(field.name, type, initial, level + 1));
                    buf.add(";");
            }
            buf.add("\n");
        }
        buf.add("}");
        return buf.toString();
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

    public function printVar(name:String, type:Null<SyntaxType>, initial:Null<Expr>, level:Int):String {
        var buf = new StringBuf();
        buf.add("var ");
        buf.add(name);
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
                    buf.add(indent(level + 1));
                    buf.add(printExpr(e, level + 1));
                    buf.add(";\n");
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

            case EVar(name, type, initial):
                printVar(name, type, initial, level);

            case EParens(e):
                '(${printExpr(e, level)})';

            case ETuple(exprs):
                switch (exprs) {
                    case []: "()";
                    case [expr]: '(${printExpr(expr, level)},)';
                    case _: '(${[for (e in exprs) printExpr(e, level)].join(", ")})';
                }
        }
    }

    public function printSyntaxType(type:SyntaxType):String {
        return switch (type) {
            case TPath(module, name):
                module.concat([name]).join(".");

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
}
