package toylang;

import toylang.Type;
import toylang.Syntax;
import toylang.DebugUtils.texprToString;
import toylang.Typer.follow;

enum Pattern {
    PConstructor(c:Constructor);
    PTuple(patterns:Array<Pattern>);
    PAny;
}

enum Constructor {
    CLiteral(l:TLiteral);
}

enum DecisionTree {
    DFail;
    DLeaf(e:Expr);
    DSwitch(subject:TExpr, cases:Array<DTCase>, def:DecisionTree);
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
    var typer:Typer;

    public function new(typer:Typer) {
        this.typer = typer;
    }

    public function match(subject:TExpr, cases:Array<Case>):DecisionTree {
        var matchCases = [];

        function fail<T>(e:Expr):T
            throw "Unrecognized pattern " + (new Printer().printExpr(e, 0));

        function parsePattern(e:Expr, t:Type):Pattern {
            return switch (e.kind) {
                case ELiteral(l):
                    var e = typer.typeLiteral(l, e.pos);
                    Typer.unifyThrow(e.type, t, e.pos);
                    var l = switch (e.kind) { case TLiteral(l): l; default: throw false; };
                    PConstructor(CLiteral(l));
                case EIdent("_"):
                    PAny;
                case ETuple(exprs):
                    var subTypes = switch (t) {
                        case TTuple(types): types;
                        case _: fail(e);
                    };
                    if (exprs.length < subTypes.length)
                        throw "Not enough arguments";
                    else if (exprs.length > subTypes.length)
                        throw "Too many arguments";
                    PTuple([for (i in 0...exprs.length) parsePattern(exprs[i], follow(subTypes[i]))]);
                case _:
                    fail(e);
            }
        }

        for (c in cases) {
            var pattern = parsePattern(c.pattern, follow(subject.type));
            matchCases.push({
                patterns: [pattern],
                expr: c.expr,
            });
        }

        function getSigma(cases:Array<MatcherCase>, type:Type):Array<Constructor> {
            var result = new haxe.ds.EnumValueMap();
            for (c in cases) {
                switch (c.patterns[0]) {
                    case PAny:
                    case PConstructor(ctor):
                        result.set(ctor, true);
                    case PTuple(_):
                        throw "unexpected tuple pattern (must be expanded before)";
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
                    case PTuple(patterns):
                        var patterns = patterns.concat(c.patterns.slice(1));
                        result.push({patterns: patterns, expr: c.expr});
                    case PConstructor(_):
                }
            }
            return result;
        }

        function getDefault(cases:Array<MatcherCase>):Array<MatcherCase> {
            var result = [];
            for (c in cases) {
                switch (c.patterns[0]) {
                    case PAny:
                        result.push({patterns: c.patterns.slice(1), expr: c.expr});
                    case PTuple(pats) if (Lambda.foreach(pats, function(p) return p.match(PAny))):
                        result.push({patterns: c.patterns.slice(1), expr: c.expr});
                    case PTuple(_):
                    case PConstructor(_):
                }
            }
            return result;
        }

        function getSubSubjects(ctor:Constructor, subject:TExpr):Array<TExpr> {
            return switch (ctor) {
                case CLiteral(_):
                    [];
            }
        }

        function expandTuple(subjects:Array<TExpr>, cases:Array<MatcherCase>) {
            var subject = subjects[0];
            switch (follow(subject.type)) {
                case TTuple(types):
                    var subSubjects = [for (i in 0...types.length) new TExpr(TTupleElement(subject, i), types[i], subject.pos)];
                    var newCases = [];
                    for (c in cases) {
                        var subPatterns =
                            switch (c.patterns[0]) {
                                case PAny:
                                    [for (_ in 0...types.length) PAny];
                                case PTuple(patterns):
                                    patterns;
                                case p:
                                    throw 'Unexpected pattern $p';
                            }
                        newCases.push({
                            expr: c.expr,
                            patterns: subPatterns.concat(c.patterns.slice(1))
                        });
                    }
                    return {subjects: subSubjects.concat(subjects.slice(1)), cases: newCases};
                case _:
                    return null;
            }
        }

        function isWildCardPattern(p:Pattern):Bool {
            return switch (p) {
                case PAny:
                    true;
                case PTuple(pl):
                    for (p in pl) {
                        if (!isWildCardPattern(p))
                            return false;
                    }
                    true;
                case PConstructor(_):
                    false;
            }
        }

        function selectColumn(subjects:Array<TExpr>, cases:Array<MatcherCase>) {
            var numCols = cases[0].patterns.length;
            var scores = [for (i in 0...numCols) 0];
            var first = true;
            for (c in cases) {
                for (i in 0...numCols) {
                    switch (c.patterns[i]) {
                        case PAny:
                        case PTuple(pats) if (Lambda.foreach(pats, isWildCardPattern)):
                        case PConstructor(_) | PTuple(_):
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

            if (col > 0) {
                // swap columns
                var subj = subjects[col];
                subjects[col] = subjects[0];
                subjects[0] = subj;
                for (c in cases) {
                    var pat = c.patterns[col];
                    c.patterns[col] = c.patterns[0];
                    c.patterns[0] = pat;
                }
            }
            return {subjects: subjects, cases: cases};
        }


        function compile(subjects:Array<TExpr>, cases:Array<MatcherCase>):DecisionTree {
            return if (cases.length == 0) {
                DFail;
            } else if (Lambda.foreach(cases[0].patterns, isWildCardPattern)) {
                DLeaf(cases[0].expr);
            } else {
                while (true) {
                    // swap columns if needed
                    var r = selectColumn(subjects, cases);
                    subjects = r.subjects;
                    cases = r.cases;

                    // expand tuple in first column
                    var r = expandTuple(subjects, cases);
                    if (r == null) {
                        // if there was no tuple - start matching first column normally
                        break;
                    }

                    // if it was tuple as well - continue selecting best column in the next loop cycle
                    subjects = r.subjects;
                    cases = r.cases;
                }

                var subject = subjects[0];
                var subjects = subjects.slice(1);

                switch (follow(subject.type)) {
                    case TTuple(types):
                        var subSubjects = [for (i in 0...types.length) new TExpr(TTupleElement(subject, i), types[i], subject.pos)];
                        subject = subSubjects[0];
                        subjects = subSubjects.slice(1).concat(subjects);
                        var newCases = [];
                        for (c in cases) {
                            switch (c.patterns[0]) {
                                case PTuple(patterns):
                                    newCases.push({patterns: patterns.concat(c.patterns.slice(1)), expr: c.expr});
                                case _:
                                    newCases.push(c);
                            }
                        }
                        cases = newCases;
                    case _:
                }

                var sigma = getSigma(cases, subject.type);
                var ctorCases = [];
                for (ctor in sigma) {
                    var spec = getSpecialized(ctor, cases);
                    var subjects = getSubSubjects(ctor, subject).concat(subjects);
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

    static function ctorToString(c:Constructor):String {
        return switch (c) {
            case CLiteral(l):
                'Literal(${DebugUtils.literalToString(l)})';
        }
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
                        edges.push({from: nodeId, to: caseId, label: '${ctorToString(c.ctor)}'});
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
