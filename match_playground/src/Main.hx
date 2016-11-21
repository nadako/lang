import haxe.Json.stringify as json;

typedef Ctor = Int;
typedef Expr = String;
typedef Subj = String;

enum Pattern {
    PAny;
    PCtor(ctor:Ctor);
}

typedef Case = {
    patterns:Array<Pattern>,
    expr:Expr,
}

typedef M = Array<Case>;

enum DT {
    Leaf(expr:Expr);
    Switch(o:Subj, cases:Array<{ctor:Ctor, dt:DT}>, def:DT);
    Fail;
}

class Main {
    static function main() {
        haxe.Log.trace = haxe.Log.trace;

        var subjects = ["x",      "y"];
        var cases:M = [
            {patterns: [PCtor(1), PCtor(2)], expr: "expr1"},
            {patterns: [PCtor(2), PAny    ], expr: "expr2"},
            {patterns: [PAny,     PCtor(3)], expr: "expr3"},
            {patterns: [PAny,     PAny    ], expr: "expr4"},
        ];

        function printMatrix(m:M) {
            for (c in m) {
                trace([for (p in c.patterns) StringTools.rpad(""+p, " ", 8)].join(" , ") + " => " + c.expr);
            }
            trace("---");
        }

        function getSigma(m:M):Array<Ctor> {
            var ctors = new Map();
            for (c in m) {
                switch (c.patterns[0]) {
                    case PAny:
                    case PCtor(c):
                        ctors[c] = true;
                }
            }
            return [for (c in ctors.keys()) c];
        }

        function getSpec(ctor:Ctor, m:M):M {
            var r = [];
            for (c in m) {
                switch (c.patterns[0]) {
                    case PAny:
                        r.push({patterns: c.patterns.slice(1), expr: c.expr});
                    case PCtor(thatCtor) if (thatCtor == ctor):
                        r.push({patterns: c.patterns.slice(1), expr: c.expr});
                    case _:
                }
            }
            return r;
        }

        function getDef(m:M):M {
            var r = [];
            for (c in m) {
                switch (c.patterns[0]) {
                    case PCtor(_):
                    case PAny:
                        r.push({patterns: c.patterns.slice(1), expr: c.expr});
                }
            }
            return r;
        }

        // first row + small default, i guess
        function chooseColumn(m:M):Int {
            var numCols = m[0].patterns.length;
            var scores = [for (i in 0...numCols) 0];
            var first = true;
            for (c in m) {
                for (i in 0...numCols) {
                    switch (c.patterns[i]) {
                        case PAny:
                        case PCtor(_):
                            if (first || scores[i] > 0)
                                scores[i]++;
                    }
                }
                first = false;
            }
            var col = 0;
            for (i in 1...numCols) {
                if (scores[i] > scores[col])
                    col = i;
            }
            return col;
        }

        function sw(subjects:Array<Subj>, cases:M):DT {
            return if (cases.length == 0) {
                Fail;
            } else if (Lambda.foreach(cases[0].patterns, function(p) return p.match(PAny))) {
                Leaf(cases[0].expr);
            } else {
                var needed = chooseColumn(cases);
                if (needed > 0) {
                    // swap columns
                    var subj = subjects[needed];
                    subjects[needed] = subjects[0];
                    subjects[0] = subj;
                    for (c in cases) {
                        var pat = c.patterns[needed];
                        c.patterns[needed] = c.patterns[0];
                        c.patterns[0] = pat;
                    }
                }
                var ctors = getSigma(cases);
                var subj = subjects[0];
                var rest = subjects.slice(1);
                var ctorCases = [];
                for (ctor in ctors) {
                    var spec = getSpec(ctor, cases);
                    var dt = sw(rest, spec);
                    ctorCases.push({ctor: ctor, dt: dt});
                }
                var defCases = getDef(cases);
                var def = sw(rest, defCases);
                Switch(subj, ctorCases, def);
            }
        }

        var dt = sw(subjects, cases);

        var nextId = 0;
        var nodes = [];
        var edges = [];
        function loop(dt:DT):Int {
            var nodeId = nextId++;
            switch (dt) {
                case Fail:
                    nodes.push({id: nodeId, label: "Fail"});
                case Leaf(expr):
                    nodes.push({id: nodeId, label: 'Leaf($expr)'});
                case Switch(subj, cases, def):
                    nodes.push({id: nodeId, label: 'Switch($subj)'});
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


        var nodes = [for (n in nodes) '${n.id} [label=${json(n.label)}];'];
        var edges = [for (e in edges) '${e.from} -> ${e.to} [label=${json(e.label)}];'];
        var dot = 'digraph dt {\ngraph [rankdir=LR];\n${nodes.join("\n")}\n${edges.join("\n")}\n}';

        sys.io.File.saveContent('dt.dot', dot);
        Sys.command("C:/Program Files (x86)/Graphviz2.38/bin/dot.exe", ['dt.dot', '-odt.png', "-Tpng"]);
    }
}
