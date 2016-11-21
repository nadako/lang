package toylang;

import toylang.Type;
import toylang.Syntax;

enum Pattern {
    PConstructor(c:Constructor, pl:Array<Pattern>);
    PAny;
}

enum Constructor {
    CLiteral(l:Literal);
}

class Matcher {
    public function new() {
    }

    public function match(subject:TExpr, cases:Array<Case>) {
        var matchCases = [];
        for (c in cases) {
            var pattern =
                switch (c.pattern.kind) {
                    case ELiteral(l):
                        PConstructor(CLiteral(l), []);
                    case EIdent("_"):
                        PAny;
                    case _:
                        throw "Unrecognized pattern " + (new Printer().printExpr(c.pattern, 0));
                }
            matchCases.push({
                pattern: pattern,
                expr: c.expr,
            });
        }

        trace(matchCases);
    }
}
