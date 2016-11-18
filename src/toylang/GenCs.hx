package toylang;

import toylang.Type;

class GenCs {
    var buf:StringBuf;

    public function new() {
    }

    public function generate(decls:Array<TDecl>):String {
        buf = new StringBuf();
        buf.add("class Module {\n");
        for (decl in decls) {
            switch (decl) {
                case TDFunction(fun):
                    generateFunction(fun, 1);
                case _:
            }
        }
        buf.add("}\n");
        return buf.toString();
    }

    function typeToString(t:Type):String {
        return switch (t) {
            case TConst(t): typeToString(t);
            case TMono(m): typeToString(m.type);
            case TInst(cl):
                switch [cl.module, cl.name] {
                    case [[], "Void"]: "void";
                    case [[], "String"]: "string";
                    case [[], "Int"]: "int";
                    case [[], "Bool"]: "bool";
                    case _: throw "todo" + cl;
                }
            case TFun(_) | TTuple(_): throw "todo " + t;
        }
    }

    function generateFunction(fun:TFunctionDecl, level:Int) {
        if (fun.cfg == null)
            return;
        indent(level);
        buf.add("static ");
        buf.add(typeToString(fun.ret));
        buf.add(" ");
        buf.add(fun.name);
        buf.add("(");
        generateSeparated(fun.args, function(arg) buf.add(arg.name), ", ");
        buf.add(") {\n");
        generateBlock(fun.cfg, level + 1);
        indent(level);
        buf.add("}\n");
    }

    function generateBlock(bb:BasicBlock, level:Int) {
        switch (bb.syntaxEdge) {
            case SENone:
                for (e in bb.elements)
                    generateStatement(e, level);

            case SELoop(head, body, next):
                for (e in bb.elements)
                    generateStatement(e, level);
                generateLoop(head, body, level);
                generateBlock(next, level);

            case SEBranch(then, els, next):
                for (i in 0...bb.elements.length - 1)
                    generateStatement(bb.elements[i], level);
                generateBranch(bb.elements[bb.elements.length - 1], then, els, level);
                generateBlock(next, level);
        }
    }

    function generateBranch(econd:TExpr, then:BasicBlock, els:Null<BasicBlock>, level:Int) {
        indent(level);
        buf.add("if (");
        generateExpr(econd);
        buf.add(") {\n");
        generateBlock(then, level + 1);
        indent(level);
        buf.add("}");
        if (els != null) {
            buf.add(" else {\n");
            generateBlock(els, level + 1);
            indent(level);
            buf.add("}\n");
        } else {
            buf.add("\n");
        }
    }

    function generateLoop(head:BasicBlock, body:BasicBlock, level:Int) {
        indent(level);
        buf.add("while (true) {\n");

        for (i in 0...head.elements.length - 1)
            generateStatement(head.elements[i], level + 1);

        indent(level + 1);
        buf.add("if (!(");
        generateExpr(head.elements[head.elements.length - 1]);
        buf.add(")) break;\n");

        generateBlock(body, level + 1);

        indent(level);
        buf.add("}\n");
    }

    function indent(level:Int) {
        for (_ in 0...level)
            buf.add("\t");
    }

    function generateStatement(e:TExpr, level:Int) {
        indent(level);
        generateExpr(e);
        buf.add(";\n");
    }

    function fieldName(fa:FieldAccess):String {
        return switch (fa) {
            case FClassField(_, f): f.name;
        }
    }

    function generateExpr(e:TExpr) {
        switch (e.kind) {
            case TThis:
                buf.add("this");
            case TLiteral(LInt(i)):
                buf.add(i);
            case TLiteral(LString(s)):
                buf.add(haxe.Json.stringify(s));
            case TLiteral(LBool(b)):
                buf.add(if (b) "true" else "false");
            case TLocal(v):
                var name = if (v.name == "trace") "global::System.Console.WriteLine" else v.name;
                buf.add(name);
            case TVar(v, evalue):
                buf.add(typeToString(v.type));
                buf.add(" ");
                buf.add(v.name);
                if (evalue != null) {
                    buf.add(" = ");
                    generateExpr(evalue);
                }
            case TAssign(ATVar(v), evalue):
                buf.add(v.name);
                buf.add(" = ");
                generateExpr(evalue);
            case TAssign(ATField(eobj, f), evalue):
                generateExpr(eobj);
                buf.add(".");
                buf.add(fieldName(f));
                buf.add(" = ");
                generateExpr(evalue);
            case TVarField(eobj, f) | TMethodClosure(eobj, f):
                generateExpr(eobj);
                buf.add(".");
                buf.add(fieldName(f));
            case TCall(eobj, args):
                generateExpr(eobj);
                buf.add("(");
                generateSeparated(args, generateExpr, ", ");
                buf.add(")");
            case TMethodCall(eobj, f, args):
                generateExpr(eobj);
                buf.add(".");
                buf.add(fieldName(f));
                buf.add("(");
                generateSeparated(args, generateExpr, ", ");
                buf.add(")");
            case TReturn(v):
                buf.add("return");
                if (v != null) {
                    buf.add(" ");
                    generateExpr(v);
                }
            case TTuple(values):
                throw "todo";
            case TNew(_) | TFunction(_):
                throw "todo " + e;
            case TFakeValue:
                throw "unexpected " + e.kind.getName();
        }
    }

    function generateSeparated<T>(exprs:Array<T>, f:T->Void, sep:String) {
        var fst = true;
        for (e in exprs) {
            if (fst)
                fst = false;
            else
                buf.add(sep);
            f(e);
        }
    }
}
