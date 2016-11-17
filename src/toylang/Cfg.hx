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
}

class CfgBuilder {
    var typer:Typer;

    public function new(typer:Typer) {
        this.typer = typer;
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
            case TLocal(v):
                bb.addElement(e);
                bb;
            default:
                throw "todo " + e;
        }
    }

    function value(bb:BasicBlock, e:TExpr):{bb:BasicBlock, expr:TExpr} {
        return switch (e.kind) {
            default:
                {bb: bb, expr: e};
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
            case TLiteral(LString(s)):
                '"${Lexer.escapeString(s)}"';
            default:
                throw "todo" + e;
        }
    }
}
