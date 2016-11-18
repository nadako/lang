package toylang;

import haxe.ds.GenericStack;
import toylang.Type;

class Edge {
    public var to:BasicBlock;
    public var label:String;

    public function new(to:BasicBlock, label:String) {
        this.to = to;
        this.label = label;
    }
}

enum SyntaxEdge {
    SEBranch(bbThen:BasicBlock, bbElse:Null<BasicBlock>, bbNext:BasicBlock);
    SELoop(head:BasicBlock, bbBody:BasicBlock, bbNext:BasicBlock);
    SENone;
}

class BasicBlock {
    public var id:Int;
    public var elements:Array<TExpr>;
    public var edges:Array<Edge>;
    public var syntaxEdge:SyntaxEdge;

    static var nextId = 0;

    public function new() {
        id = nextId++;
        elements = [];
        edges = [];
        syntaxEdge = SENone;
    }

    public inline function addElement(e:TExpr) {
        elements.push(e);
    }

    public function addEdge(to:BasicBlock, label:String) {
        edges.push(new Edge(to, label));
    }
}

class UnreachableBlock extends BasicBlock {
    override function addEdge(_, _) {}
}

class LoopContext {
    public var head:BasicBlock;
    public var next:BasicBlock;

    public function new(head:BasicBlock, next:BasicBlock) {
        this.head = head;
        this.next = next;
    }
}

class CfgBuilder {
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

    public static function makeDotGraph(root:BasicBlock):String {
        var blocks = [];
        var edges = [];
        function walk(bb:BasicBlock) {
            blocks.push({id: bb.id, label: haxe.Json.stringify('<${bb.id}>\n' + bb.elements.map(texprToString).join("\n"))});
            for (edge in bb.edges) {
                edges.push({from: bb.id, to: edge.to.id, label: haxe.Json.stringify(edge.label)});
                if (!Lambda.exists(blocks, function(b) return b.id == edge.to.id))
                    walk(edge.to);
            }
        }
        walk(root);

        var blocks = [for (b in blocks) '\t${b.id} [label=${b.label}];'].join("\n");
        var edges = [for (e in edges) '\t${e.from} -> ${e.to} [label=${e.label}];'].join("\n");
        return 'digraph cfg {\n$blocks\n$edges\n}';
    }

    public static function makeVisJsGraph(root:BasicBlock) {
        var blocks = [];
        var edges = [];
        function walk(bb:BasicBlock, level:Int) {
            blocks.push({id: bb.id, label: '<${bb.id}>\n' + bb.elements.map(texprToString).join("\n"), level: level});
            for (edge in bb.edges) {
                edges.push({from: bb.id, to: edge.to.id, label: edge.label, arrows: "to"});
                if (!Lambda.exists(blocks, function(b) return b.id == edge.to.id))
                    walk(edge.to, level + 1);
            }
        }
        walk(root, 0);

        return {
            nodes: blocks,
            edges: edges,
        };
    }

    static function texprToString(e:TExpr):String {
        return switch (e.kind) {
            case TVar(v, e):
                var s = 'var ${v.name}';
                if (e != null)
                    s += ' = ' + texprToString(e);
                s;
            case TLocal(v):
                v.name;
            case TThis:
                "this";
            case TAssign(target, e):
                var targetStr = switch (target) {
                    case ATVar(v): v.name;
                    case ATField(eobj, f):
                        var fieldName = switch (f) {
                            case FClassField(_, f): f.name;
                        };
                        texprToString(eobj) + "." + fieldName;
                }
                '$targetStr = ${texprToString(e)}';
            case TLiteral(LInt(i)):
                '$i';
            case TLiteral(LBool(b)):
                if (b) "true" else "false";
            case TLiteral(LString(s)):
                '"${Lexer.escapeString(s)}"';
            case TCall(eobj, args):
                '${texprToString(eobj)}(${args.map(texprToString).join(", ")})';
            case TTuple(values):
                var valuesStr = values.map(texprToString).join(", ");
                if (values.length == 1) valuesStr += ",";
                '($valuesStr)';
            case TMethodCall(eobj, f, args):
                var fieldName = switch (f) {
                    case FClassField(_, f): f.name;
                }
                '${texprToString(eobj)}.$fieldName(${args.map(texprToString).join(", ")})';
            case TVarField(e, f):
                var fieldName = switch (f) {
                    case FClassField(_, f): f.name;
                }
                '${texprToString(e)}.${fieldName}';
            case TMethodClosure(e, f):
                var fieldName = switch (f) {
                    case FClassField(_, f): f.name;
                }
                'METHODCLOSURE<${texprToString(e)}.${fieldName}>';
            case TNew(cl):
                var path = cl.module.concat([cl.name]).join(".");
                'new $path';
            case TReturn(e):
                if (e == null)
                    'return';
                else
                    'return ${texprToString(e)}';
            case TIf(_, _, _) | TBlock(_) | TWhile(_, _) | TBreak | TContinue | TFakeValue:
                throw 'basic block element expressions cannot contain ' + e.kind.getName();
            case TFunction(_, _):
                throw "todo" + e; // ???
        }
    }
}
