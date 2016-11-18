package toylang;

class Edge {
    public var to:BasicBlock;
    public var label:String;

    public function new(to:BasicBlock, label:String) {
        this.to = to;
        this.label = label;
    }
}
