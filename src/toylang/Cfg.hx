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
}

class CfgBuilder {
    public function new() {
    }

    public function build(e:TExpr):BasicBlock {
        var bbRoot = new BasicBlock();
        block(bbRoot, e);
        return bbRoot;
    }

    function block(bb:BasicBlock, e:TExpr):BasicBlock {
        return bb;
    }

    public static function makeDotGraph(root:BasicBlock):String {
        var blocks = [];
        var edges = [];
        function walk(bb:BasicBlock) {
            blocks.push({id: bb.id, label: haxe.Json.stringify('<${bb.id}>\n' + bb.elements.map(Std.string).join("\n"))});
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
}
