package toylang;

import toylang.Type;
import toylang.SyntaxEdge;
import toylang.Printer.printBinop;
import toylang.Printer.printUnop;

class GenJs {
    var buf:StringBuf;

    public function new() {
    }

    public function generate(decls:Array<TDecl>):String {
        buf = new StringBuf();
        for (decl in decls) {
            switch (decl) {
                case TDFunction(fun):
                    generateFunction(fun);
                case _:
            }
        }
        return buf.toString();
    }

    function generateFunction(fun:TFunctionDecl) {
        if (fun.cfg == null)
            return;
        buf.add("function ");
        buf.add(fun.name);
        buf.add("(");
        generateSeparated(fun.args, function(arg) buf.add(arg.name), ", ");
        buf.add(") {\n");
        generateBlock(fun.cfg, 1);
        buf.add("}\n\n");
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

            case SESwitch(cases, def, next):
                for (i in 0...bb.elements.length - 1)
                    generateStatement(bb.elements[i], level);
                var cond = bb.elements[bb.elements.length - 1];
                generateSwitch(cond, cases, def, level);
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

    function generateSwitch(econd:TExpr, cases:Array<SESwitchCase>, def:Null<BasicBlock>, level:Int) {
        indent(level);
        buf.add("switch (");
        generateExpr(econd);
        buf.add(") {\n");
        for (c in cases) {
            indent(level + 1);
            buf.add("case ");
            generateExpr(c.expr);
            buf.add(":\n");
            generateBlock(c.body, level + 2);
        }
        if (def != null) {
            indent(level + 1);
            buf.add("default:\n");
            generateBlock(def, level + 2);
        }
        indent(level);
        buf.add("}\n");
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
                buf.add(v.name);
            case TVar(v, evalue):
                buf.add("var ");
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
            case TVarField(eobj, f):
                generateExpr(eobj);
                buf.add(".");
                buf.add(fieldName(f));
            case TMethodClosure(eobj, f):
                buf.add("(function(o) { return o.");
                buf.add(fieldName(f));
                buf.add(".bind(o); })(");
                generateExpr(eobj);
                buf.add(")");
            case TBinop(op, left, right):
                generateExpr(left);
                buf.add(" ");
                buf.add(printBinop(op));
                buf.add(" ");
                generateExpr(right);
            case TUnop(op, expr, postfix):
                if (!postfix)
                    buf.add(printUnop(op));
                buf.add("(");
                generateExpr(expr);
                buf.add(")");
                if (postfix)
                    buf.add(printUnop(op));
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
            case TTuple(values):
                buf.add("[");
                generateSeparated(values, generateExpr, ", ");
                buf.add("]");
            case TTupleElement(eobj, index):
                generateExpr(eobj);
                buf.add("[");
                buf.add(index);
                buf.add("]");
            case TReturn(v):
                buf.add("return");
                if (v != null) {
                    buf.add(" ");
                    generateExpr(v);
                }
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
