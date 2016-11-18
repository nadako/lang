package toylang;

import toylang.Type;

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
