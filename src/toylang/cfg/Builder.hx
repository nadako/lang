package toylang.cfg;

import haxe.ds.GenericStack;
import toylang.Type;
import toylang.cfg.BasicBlock;

class LoopContext {
    public var head:BasicBlock;
    public var next:BasicBlock;

    public function new(head:BasicBlock, next:BasicBlock) {
        this.head = head;
        this.next = next;
    }
}

class Builder {
    var typer:Typer;
    var tmpCount:Int;
    var loopStack:GenericStack<LoopContext>;
    var bbUnreachable:BasicBlock;

    public function new(typer:Typer) {
        this.typer = typer;
        tmpCount = 0;
        loopStack = new GenericStack();
        bbUnreachable = new UnreachableBlock();
    }

    public function build(e:TExpr):BasicBlock {
        var bbRoot = new BasicBlock();
        block(bbRoot, e);
        return bbRoot;
    }

    function block(bb:BasicBlock, e:TExpr):BasicBlock {
        var exprs = switch (e.kind) {
            case TBlock(exprs): exprs;
            default: [e];
        }
        for (e in exprs)
            bb = blockElement(bb, e);
        return bb;
    }

    function blockElement(bb:BasicBlock, e:TExpr):BasicBlock {
        return switch (e.kind) {
            case TVar(v, einitial):
                declareVar(bb, v, e.pos);
                if (einitial != null) {
                    var r = value(bb, einitial);
                    bb = r.bb;
                    assignVar(bb, v, r.expr, e.pos);
                }
                bb;

            case TBlock([]):
                // who needs empty blocks?
                bb;

            case TBlock([e]):
                blockElement(bb, e);

            case TBlock(exprs):
                for (e in exprs)
                    bb = blockElement(bb, e);
                bb;

            case TLiteral(_) | TLocal(_) | TThis:
                bb.addElement(e);
                bb;

            case TNew(_):
                // right now, we don't support constructors,
                // so this is a no-sideeffect expr and could actually be not added
                bb.addElement(e);
                bb;

            case TIf(econd, ethen, eelse):
                var r = value(bb, econd);
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

            case TCall(eobj, args):
                var r = call(bb, eobj, args, e.type, e.pos);
                r.bb.addElement(r.expr);
                r.bb;

            case TMethodCall(eobj, f, args):
                var r = methodCall(bb, eobj, f, args, e.type, e.pos);
                r.bb.addElement(r.expr);
                r.bb;

            case TAssign(_, _):
                var r = value(bb, e);
                r.bb.addElement(r.expr);
                r.bb;

            case TMethodClosure(_, _) | TVarField(_, _) | TTuple(_):
                var r = value(bb, e);
                // it doesn't really make sense to add it to the block
                r.bb.addElement(r.expr);
                r.bb;

            case TWhile(econd, ebody):
                var bbLoopHead = new BasicBlock();
                var r = value(bbLoopHead, econd);
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

            case TContinue:
                var loopCtx = loopStack.first();
                if (loopCtx == null)
                    throw "continue outside of loop";
                bb.addEdge(loopCtx.head, "continue");
                bbUnreachable;

            case TBreak:
                var loopCtx = loopStack.first();
                if (loopCtx == null)
                    throw "break outside of loop";
                bb.addEdge(loopCtx.next, "break");
                bbUnreachable;

            case TReturn(rvalue):
                if (rvalue == null) {
                    bb.addElement(e);
                } else {
                    var r = value(bb, rvalue);
                    r.bb.addElement(new TExpr(TReturn(r.expr), e.type, e.pos));
                }
                bbUnreachable;

            case TFunction(_, _):
                throw "todo " + e;

            case TFakeValue:
                throw "unexpected fake value";
        }
    }

    // TODO: merge this and methodCall
    function call(bb:BasicBlock, eobj:TExpr, args:Array<TExpr>, ret:Type, pos:Position):{bb:BasicBlock, expr:TExpr} {
        var r = value(bb, eobj);
        eobj = r.expr;
        bb = r.bb;
        var valueArgs = [];
        for (e in args) {
            var r = value(bb, e);
            bb = r.bb;
            valueArgs.push(r.expr);
        }
        return {bb: bb, expr: new TExpr(TCall(eobj, valueArgs), ret, pos)}
    }

    function methodCall(bb:BasicBlock, eobj:TExpr, f:FieldAccess, args:Array<TExpr>, ret:Type, pos:Position):{bb:BasicBlock, expr:TExpr} {
        var r = value(bb, eobj);
        eobj = r.expr;
        bb = r.bb;
        var valueArgs = [];
        for (e in args) {
            var r = value(bb, e);
            bb = r.bb;
            valueArgs.push(r.expr);
        }
        return {bb: bb, expr: new TExpr(TMethodCall(eobj, f, valueArgs), ret, pos)}
    }

    function value(bb:BasicBlock, e:TExpr):{bb:BasicBlock, expr:TExpr} {
        return switch (e.kind) {
            case TVar(_, _):
                throw "var declaration is not allowed in a value place";

            case TWhile(_, _):
                throw "while loop is not allowed in a value place";

            case TBlock([]):
                // this shouldn't happen
                throw "empty blocks are not allowed in a value place";

            case TBlock([e]):
                value(bb, e);

            case TBlock(el):
                var last = el.pop();
                for (e in el)
                    bb = blockElement(bb, e);
                value(bb, last);

            case TLiteral(_) | TLocal(_) | TThis:
                {bb: bb, expr: e};

            case TNew(_):
                // right now we don't support constructors, so no need to
                // think about constructor arguments :)
                {bb: bb, expr: e};

            case TMethodClosure(eobj, f):
                var r = value(bb, eobj);
                {bb: r.bb, expr: new TExpr(TMethodClosure(r.expr, f), e.type, e.pos)};

            case TCall(eobj, args):
                call(bb, eobj, args, e.type, e.pos);

            case TMethodCall(eobj, f, args):
                methodCall(bb, eobj, f, args, e.type, e.pos);

            case TVarField(eobj, f):
                var r = value(bb, eobj);
                {bb: r.bb, expr: new TExpr(TVarField(r.expr, f), e.type, e.pos)};

            case TAssign(ATVar(v), evalue):
                var r = value(bb, evalue);
                {bb: r.bb, expr: new TExpr(TAssign(ATVar(v), r.expr), e.type, e.pos)};

            case TAssign(ATField(eobj, f), evalue):
                var r = value(bb, eobj);
                var eobj = r.expr;
                var r = value(r.bb, evalue);
                {bb: r.bb, expr: new TExpr(TAssign(ATField(eobj, f), r.expr), e.type, e.pos)};

            case TTuple(exprs):
                var valueExprs = [];
                for (e in exprs) {
                    var r = value(bb, e);
                    bb = r.bb;
                    valueExprs.push(r.expr);
                }
                {bb: bb, expr: new TExpr(TTuple(valueExprs), e.type, e.pos)};

            case TIf(econd, ethen, eelse):
                if (eelse == null)
                    throw "if in a value place must have else branch";

                var tmpVar = new TVar("tmp" + (tmpCount++), e.type);
                declareVar(bb, tmpVar, e.pos);

                var r = value(bb, econd);
                r.bb.addElement(r.expr);

                var bbNext = new BasicBlock();
                var bbThen = new BasicBlock();
                var bbElse = new BasicBlock();
                {
                    r.bb.addEdge(bbThen, "then");
                    var r = value(bbThen, ethen);
                    assignVar(r.bb, tmpVar, r.expr, e.pos);
                    r.bb.addEdge(bbNext, "next");
                }
                {
                    r.bb.addEdge(bbElse, "else");
                    var r = value(bbElse, eelse);
                    assignVar(r.bb, tmpVar, r.expr, e.pos);
                    r.bb.addEdge(bbNext, "next");
                }

                r.bb.syntaxEdge = SEBranch(bbThen, bbElse, bbNext);

                {bb: bbNext, expr: new TExpr(TLocal(tmpVar), tmpVar.type, e.pos)};

            case TFunction(_, _):
                throw "todo " + e;

            case TBreak | TContinue | TReturn(_):
                var bb = blockElement(bb, e);
                {bb: bb, expr: new TExpr(TFakeValue, e.type, e.pos)};

            case TFakeValue:
                throw "unexpected fake value";
        }
    }

    function assignVar(bb:BasicBlock, v:TVar, e:TExpr, pos:Position) {
        bb.addElement(new TExpr(TAssign(ATVar(v), e), v.type, pos));
    }

    function declareVar(bb:BasicBlock, v:TVar, pos:Position) {
        bb.addElement(new TExpr(TVar(v, null), typer.tVoid, pos));
    }
}
