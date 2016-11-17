package toylang;

import toylang.Cfg.BasicBlock;
import toylang.Type;

class GenJs {
    var buf:StringBuf;

    public function new() {
    }

    public function generate(bbRoot:BasicBlock):String {
        buf = new StringBuf();

        for (e in bbRoot.elements) {
            generateStatement(e);
        }

        return buf.toString();
    }

    function generateStatement(e:TExpr) {
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
