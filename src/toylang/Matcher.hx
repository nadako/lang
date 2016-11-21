package toylang;

import toylang.Type;
import toylang.Syntax;
import toylang.DebugUtils.texprToString;

enum Pattern {
    PConstructor(c:Constructor);
    PAny;
}

enum Constructor {
    CLiteral(l:Literal);
}

enum DecisionTree {
    DFail;
    DLeaf(e:Expr);
    DSwitch(subjects:TExpr, cases:Array<DTCase>, def:DecisionTree);
}

class DTCase {
    public var ctor:Constructor;
    public var dt:DecisionTree;

    public function new(ctor, dt) {
        this.ctor = ctor;
        this.dt = dt;
    }
}

typedef MatcherCase = {
    expr:Expr,
    patterns:Array<Pattern>,
}

class Matcher {
    public function new() {
    }

    public function match(subject:TExpr, cases:Array<Case>):DecisionTree {
        var matchCases = [];
        for (c in cases) {
            var pattern =
                switch (c.pattern.kind) {
                    case ELiteral(l):
                        PConstructor(CLiteral(l));
                    case EIdent("_"):
                        PAny;
                    case _:
                        throw "Unrecognized pattern " + (new Printer().printExpr(c.pattern, 0));
                }
            matchCases.push({
                patterns: [pattern],
                expr: c.expr,
            });
        }

        function getSigma(cases:Array<MatcherCase>):Array<Constructor> {
            var result = new haxe.ds.EnumValueMap();
            for (c in cases) {
                switch (c.patterns[0]) {
                    case PAny:
                    case PConstructor(ctor):
                        result.set(ctor, true);
                }
            }
            return [for (c in result.keys()) c];
        }

        function getSpecialized(ctor:Constructor, cases:Array<MatcherCase>):Array<MatcherCase> {
            var result = [];
            for (c in cases) {
                switch (c.patterns[0]) {
                    case PAny:
                        result.push({patterns: c.patterns.slice(1), expr: c.expr});
                    case PConstructor(thatCtor) if (std.Type.enumEq(ctor, thatCtor)):
                        result.push({patterns: c.patterns.slice(1), expr: c.expr});
                    case _:
                }
            }
            return result;
        }

        function getDefault(cases:Array<MatcherCase>):Array<MatcherCase> {
            var result = [];
            for (c in cases) {
                switch (c.patterns[0]) {
                    case PConstructor(_):
                    case PAny:
                        result.push({patterns: c.patterns.slice(1), expr: c.expr});
                }
            }
            return result;
        }

        function compile(subjects:Array<TExpr>, cases:Array<MatcherCase>):DecisionTree {
            return if (cases.length == 0) {
                DFail;
            } else if (Lambda.foreach(cases[0].patterns, function(p) return p.match(PAny))) {
                DLeaf(cases[0].expr);
            } else {
                var sigma = getSigma(cases);
                var subject = subjects[0];
                var subjects = subjects.slice(1);
                var ctorCases = [];
                for (ctor in sigma) {
                    var spec = getSpecialized(ctor, cases);
                    var dt = compile(subjects, spec);
                    ctorCases.push(new DTCase(ctor, dt));
                }
                var defCases = getDefault(cases);
                var def = compile(subjects, defCases);
                DSwitch(subject, ctorCases, def);
            }
        }

        var dt = compile([subject], matchCases);
        makeDTGraph(dt);
        return dt;
    }


    static function makeDTGraph(dt:DecisionTree) {
        var nextId = 0;
        var nodes = [];
        var edges = [];
        function loop(dt:DecisionTree):Int {
            var nodeId = nextId++;
            switch (dt) {
                case DFail:
                    nodes.push({id: nodeId, label: "Fail"});
                case DLeaf(expr):
                    nodes.push({id: nodeId, label: 'Leaf(${new Printer().printExpr(expr, 0)})'});
                case DSwitch(subj, cases, def):
                    nodes.push({id: nodeId, label: 'Switch(${texprToString(subj)})'});
                    for (c in cases) {
                        var caseId = loop(c.dt);
                        edges.push({from: nodeId, to: caseId, label: '${c.ctor}'});
                    }
                    var defId = loop(def);
                    edges.push({from: nodeId, to: defId, label: 'default'});
            }
            return nodeId;
        }
        loop(dt);

        inline function json(v) return haxe.Json.stringify(v);
        var nodes = [for (n in nodes) '${n.id} [label=${json(n.label)}];'];
        var edges = [for (e in edges) '${e.from} -> ${e.to} [label=${json(e.label)}];'];
        var dot = 'digraph dt {\ngraph [rankdir=LR];\n${nodes.join("\n")}\n${edges.join("\n")}\n}';

        #if (sys || hxnodejs)
        sys.io.File.saveContent('dt.dot', dot);
        Sys.command("C:/Program Files (x86)/Graphviz2.38/bin/dot.exe", ['dt.dot', '-odt.png', "-Tpng"]);
        #end
    }
}
