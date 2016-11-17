package toylang;

import toylang.Type;

class Edge {
    public var to:BasicBlock;
    public var label:String;

    public function new(to:BasicBlock, label:String) {
        this.to = to;
        this.label = label;
    }
}

class BasicBlock {
    public var id:Int;
    public var elements:Array<TExpr>;
    public var edges:Array<Edge>;

    static var nextId = 0;

    public function new() {
        id = nextId++;
        elements = [];
        edges = [];
    }

    public inline function addElement(e:TExpr) {
        elements.push(e);
    }

    public inline function addEdge(to:BasicBlock, label:String) {
        edges.push(new Edge(to, label));
    }
}

class CfgBuilder {
    var typer:Typer;
    var tmpCount:Int;

    public function new(typer:Typer) {
        this.typer = typer;
        tmpCount = 0;
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
            case TLiteral(_) | TLocal(_):
                bb.addElement(e);
                bb;
            case TAssign(ATVar(v), evalue):
                var r = value(bb, evalue);
                bb = r.bb;
                assignVar(bb, v, r.expr, e.pos);
                bb;
            case TIf(econd, ethen, eelse):
                var r = value(bb, econd);
                r.bb.addElement(r.expr);

                var bbNext = new BasicBlock();

                var bbThen = new BasicBlock();
                r.bb.addEdge(bbThen, "then");
                bbThen = block(bbThen, ethen);
                bbThen.addEdge(bbNext, "next");

                if (eelse == null) {
                    r.bb.addEdge(bbNext, "else");
                } else {
                    var bbElse = new BasicBlock();
                    r.bb.addEdge(bbElse, "else");
                    bbElse = block(bbElse, eelse);
                    bbElse.addEdge(bbNext, "next");
                }

                bbNext;

            case TCall(eobj, args):
                var r = call(bb, eobj, args, e.type, e.pos);
                r.bb.addElement(r.expr);
                r.bb;

            default:
                throw "todo " + e;
        }
    }

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
            case TCall(eobj, args):
                call(bb, eobj, args, e.type, e.pos);
            case TIf(econd, ethen, eelse):
                if (eelse == null)
                    throw "if in a value place must have else branch";

                var tmpVar = new TVar("tmp" + (tmpCount++), e.type);
                declareVar(bb, tmpVar, e.pos);

                var r = value(bb, econd);
                r.bb.addElement(r.expr);

                var bbNext = new BasicBlock();
                {
                    var bbThen = new BasicBlock();
                    r.bb.addEdge(bbThen, "then");
                    var r = value(bbThen, ethen);
                    assignVar(r.bb, tmpVar, r.expr, e.pos);
                    r.bb.addEdge(bbNext, "next");
                }
                {
                    var bbElse = new BasicBlock();
                    r.bb.addEdge(bbElse, "else");
                    var r = value(bbElse, eelse);
                    assignVar(r.bb, tmpVar, r.expr, e.pos);
                    r.bb.addEdge(bbNext, "next");
                }

                {bb: bbNext, expr: new TExpr(TLocal(tmpVar), tmpVar.type, e.pos)};
            default:
                throw "todo " + e;
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

    static function texprToString(e:TExpr):String {
        return switch (e.kind) {
            case TVar(v, e):
                var s = 'var ${v.name}';
                if (e != null)
                    s += ' = ' + texprToString(e);
                s;
            case TLocal(v):
                v.name;
            case TAssign(ATVar(v), e):
                '${v.name} = ${texprToString(e)}';
            case TLiteral(LInt(i)):
                '$i';
            case TLiteral(LBool(b)):
                if (b) "true" else "false";
            case TLiteral(LString(s)):
                '"${Lexer.escapeString(s)}"';
            case TCall(eobj, args):
                '${texprToString(eobj)}(${args.map(texprToString).join(", ")})';
            default:
                throw "todo" + e;
        }
    }
}
