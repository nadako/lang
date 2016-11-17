enum Expr {
    Block(exprs:Array<Expr>);
    Print(arg:Expr);
    If(econd:Expr, ethen:Expr, eelse:Null<Expr>);
    Value(v:String);
    Var(name:String, value:Null<Expr>);
    Assign(target:Expr, value:Expr);
    Local(name:String);
    While(econd:Expr, ebody:Expr);
}

class Main {
    static var tmpCount = 0;
    static var i = 5;
    static function main() {
        /**
            {
                print("hi");
                while({print("sup"); cond}) print("hi");
                print("bye");
            }
        **/
        var expr = Block([
            Print(Value("hi")),
            While(Block([Print(Value("sup")), Value("cond")]), Block([
                Print(Value("Hi")),
                If(Value("cond"), Print(Value("1")),null),
            ])),
            Print(Value("bye")),
        ])

        ;
































        var bbRoot = new BasicBlock();
        var bbEnd = block(bbRoot, expr);
        // trace(bbRoot.toString(0));

        var graph = makeDotGraph(bbRoot);
        sys.io.File.saveContent("graph.dot", graph);
        Sys.command("C:/Program Files (x86)/Graphviz2.38/bin/dot.exe", ["graph.dot", "-ograph.png", "-Tpng"]);
    }

    static function makeDotGraph(root:BasicBlock):String {
        var blocks = [];
        var edges = [];
        function walk(bb:BasicBlock) {
            blocks.push({id: bb.id, label: haxe.Json.stringify('<${bb.id+(if (bb.label.length > 0)":"+bb.label else "")}>\n' + bb.elements.map(Std.string).join("\n"))});
            for (edge in bb.edges) {
                edges.push({from: bb.id, to: edge.to.id, label: haxe.Json.stringify(edge.name)});
                if (!Lambda.exists(blocks, function(b) return b.id == edge.to.id))
                    walk(edge.to);
            }
        }
        walk(root);

        var blocks = [for (b in blocks) '\t${b.id} [label=${b.label}];'].join("\n");
        var edges = [for (e in edges) '\t${e.from} -> ${e.to} [label=${e.label}];'].join("\n");
        return 'digraph cfg {\n$blocks\n$edges\n}';
    }

    static function block(bb:BasicBlock, e:Expr):BasicBlock {
        var el = switch (e) {
            case Block(el): el;
            case _: [e];
        }
        for (e in el) {
            bb = blockElement(bb, e);
        }
        return bb;
    }

    static function value(bb:BasicBlock, e:Expr):{bb:BasicBlock, e:Expr} {
        return switch (e) {
            case Value(_) | Local(_):
                {bb: bb, e: e};

            case Block(el):
                var last = el.pop();
                for (e in el) {
                    bb = blockElement(bb, e);
                }
                value(bb, last);

            case If(econd, ethen, eelse):
                if (eelse == null)
                    throw "if in value place must have else branch";
                var name = "tmp" + (tmpCount++);
                bb.declareVar(name);
                var res = value(bb, econd);
                bb.addElement(res.e);
                var bbThen = new BasicBlock();
                var bbElse = new BasicBlock();
                res.bb.addEdge("then", bbThen);
                res.bb.addEdge("else", bbElse);
                var bbNext = new BasicBlock();
                {
                    var r = value(bbThen, ethen);
                    r.bb.assign(name, r.e);
                    r.bb.addEdge("next", bbNext);
                }
                {
                    var r = value(bbElse, eelse);
                    r.bb.assign(name, r.e);
                    r.bb.addEdge("next", bbNext);
                }
                {bb: bbNext, e: Local(name)}

            case While(_, _) | Var(_, _):
                throw "while and var is invalid in value place";

            case _:
                throw "todo";
        }
    }

    static function blockElement(bb:BasicBlock, e:Expr):BasicBlock {
        return switch (e) {
            case Value(_) | Local(_):
                bb.addElement(e);
                bb;

            case Print(expr):
                var res = value(bb, expr);
                bb = res.bb;
                bb.addElement(Print(res.e));
                bb;

            case If(econd, ethen, eelse):
                var res = value(bb, econd);
                res.bb.addElement(res.e);
                var bbThen = new BasicBlock();
                var bbNext = new BasicBlock();
                res.bb.addEdge("then", bbThen);
                bbThen.addEdge("next", bbNext);
                block(bbThen, ethen);
                if (eelse != null) {
                    var bbElse = new BasicBlock();
                    res.bb.addEdge("else", bbElse);
                    bbElse.addEdge("next", bbNext);
                    block(bbElse, eelse);
                } else {
                    res.bb.addEdge("else", bbNext);
                }
                bbNext;

            case Block(el):
                var bbSub = new BasicBlock();
                var bbNext = new BasicBlock();
                bb.addEdge("sub", bbSub);
                bbSub.addEdge("next", bbNext);
                block(bbSub, e);
                bbNext;

            case Var(name, v):
                bb.declareVar(name);
                if (v != null) {
                    var r = value(bb, v);
                    bb = r.bb;
                    bb.assign(name, r.e);
                }
                bb;

            case Assign(etarget = Local(_), evalue):
                var rvalue = value(bb, evalue);
                bb = rvalue.bb;
                bb.addElement(Assign(etarget, rvalue.e));
                bb;

            case While(econd, ebody):
                var bbLoopHead = new BasicBlock("loopHead");
                bb.addEdge("next", bbLoopHead);

                var res = value(bbLoopHead, econd);
                bbLoopHead.addElement(res.e);

                var bbLoopBody = new BasicBlock("loopBody");
                var bbLoopBodyNext = block(bbLoopBody, ebody);
                var bbNext = new BasicBlock();

                bbLoopHead.addEdge("then", bbLoopBody);
                bbLoopHead.addEdge("next", bbNext);

                bbLoopBodyNext.addEdge("loop", bbLoopHead);

                bbNext;


            case _:
                throw "unsupported " + e;
        }
    }
}

typedef Edge = {
    to:BasicBlock,
    name:String,
}

@:publicFields
class BasicBlock {
    var elements:Array<Expr>;
    var edges:Array<Edge>;
    var id:Int;
    var label:String;

    static var nextId = 0;

    function new(label = "") {
        this.label = label;
        id = nextId++;
        elements = [];
        edges = [];
    }

    function declareVar(name:String) {
        addElement(Var(name, null));
    }

    function assign(name, e) {
        addElement(Assign(Local(name), e));
    }

    function addElement(e:Expr) {
        elements.push(e);
    }

    function addEdge(name:String, to:BasicBlock) {
        edges.push({name: name, to: to});
    }

    function toString(level:Int) {
        var indent = [for (_ in 0...level) "\t"].join("");
        var b = new StringBuf();
        b.add(indent);
        b.add("BLOCK ");
        b.add(id);
        b.add("\n");
        for (e in elements) {
            b.add(indent);
            b.add("elem: ");
            b.add(e);
            b.add("\n");
        }
        for (e in edges) {
            b.add(indent);
            b.add("edge: ");
            b.add(e.name);
            b.add("\n");
            b.add(e.to.toString(level + 1));
            b.add("\n");
        }
        return b;
    }
}
