package toylang;

import toylang.Cfg.BasicBlock;
import toylang.Type;

class GenJs {
    var buf:StringBuf;

    public function new() {
    }

    public function generate(bbRoot:BasicBlock):String {
        buf = new StringBuf();
        generateBlock(bbRoot, 0);
        return buf.toString();
    }

    function generateBlock(bb:BasicBlock, level:Int) {
        switch (bb.syntaxEdge) {
            case null:
                throw "no syntax edge from " + bb.id;

            case SEEnd:
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
            case TNew(_) | TReturn(_) | TFunction(_):
                throw "todo " + e;
            case TBlock(_) | TBreak | TContinue | TFakeValue | TIf(_,_,_) | TWhile(_,_):
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
