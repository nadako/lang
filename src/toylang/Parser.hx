package toylang;

import toylang.Syntax;

class ParserError {
    public var message:ParserErrorMessage;
    public var pos:Position;

    public function new(message, pos) {
        this.message = message;
        this.pos = pos;
    }

    public function toString():String {
        return '$pos: $message';
    }
}

enum ParserErrorMessage {
    MissingSemicolon;
}

class Parser extends hxparse.Parser<hxparse.LexerTokenSource<Token>, Token> implements hxparse.ParserBuilder {
    public function new(input:byte.ByteData, file:String) {
        super(new hxparse.LexerTokenSource(new Lexer(input, file), Lexer.rule));
    }

    public function parse():Array<Decl> {
        return switch stream {
            case [decls = parseDecls([]), {kind: TkEof}]:
                decls;
        }
    }

    function parseDecls(acc:Array<Decl>):Array<Decl> {
        return switch stream {
            case [decl = parseDecl(), acc = parseDecls({acc.push(decl); acc;})]:
                acc;
            case _:
                acc;
        }
    }

    function parseDecl():Decl {
        return switch stream {
            case [f = parseFunction()]:
                var decl = new Decl();
                decl.name = f.fun.name;
                decl.pos = f.pos;
                decl.kind = DFunction(f.fun);
                decl;

            case [{kind: TkKeyword(KwdClass), pos: pmin}, {kind: TkIdent(name)}, {kind: TkBraceOpen}, fields = parseRepeat(parseClassField), {kind: TkBraceClose}]:
                var decl = new Decl();
                decl.name = name;
                decl.pos = Position.union(pmin, last.pos);
                decl.kind = DClass({
                    var cls = new ClassDecl();
                    cls.fields = fields;
                    cls;
                });
                decl;
        }
    }

    function parseClassField():Field {
        return switch stream {
            case [f = parseFunction()]:
                new Field(f.fun.name, FFun(f.fun), f.pos);

            case [v = parseVar(), _ = checkSemicolon()]:
                new Field(v.name, FVar(v.type, v.initial), v.pos);
        }
    }

    function parseFunction():{fun:FunctionDecl, pos:Position} {
        return switch stream {
            case [{kind: TkKeyword(KwdFunction), pos: pmin}, {kind: TkIdent(name)}, {kind: TkParenOpen}, args = separated(TkComma, parseFunctionArg), {kind: TkParenClose}, ret = parseOptional(parseTypeHint)]:
                var expr = parseExpect(function() return switch stream {
                    case [e = parseExprWithSemicolon()]:
                        e;
                    case [{kind: TkSemicolon}]:
                        null;
                });
                {
                    fun: {
                        var f = new FunctionDecl();
                        f.name = name;
                        f.args = args;
                        f.ret = ret;
                        f.expr = expr;
                        f;
                    },
                    pos: Position.union(pmin, last.pos)
                }
        }
    }

    function parseFunctionArg():FunctionArg {
        return switch stream {
            case [{kind: TkIdent(name)}, type = parseOptional(parseTypeHint)]:
                new FunctionArg(name, type);
        }
    }

    function parseVar():{name:String, type:Null<SyntaxType>, initial:Null<Expr>, pos:Position} {
        return switch stream {
            case [{kind: TkKeyword(KwdVar), pos: pmin}, {kind: TkIdent(name)}, type = parseOptional(parseTypeHint)]:
                var expr = switch stream {
                    case [{kind: TkEqual}, e = parseExpr()]: e;
                    case _: null;
                };
                {
                    name: name,
                    type: type,
                    initial: expr,
                    pos: Position.union(pmin, last.pos)
                }
        }
    }

    function parseTypeHint():SyntaxType {
        return switch stream {
            case [{kind: TkColon}, type = parseSyntaxType()]:
                type;
        }
    }

    function parseSyntaxType():SyntaxType {
        return switch stream {
            case [{kind: TkParenOpen}]:
                switch stream {
                    case [{kind: TkParenClose}]:
                        switch stream {
                            case [{kind: TkArrow}, ret = parseSyntaxType()]:
                                TFunction([], ret);
                            case _:
                                TTuple([]);
                        }
                    case [t = parseSyntaxType()]:
                        switch stream {
                            case [{kind: TkParenClose}]:
                                parseExpect(function() return switch stream {
                                    case [{kind: TkArrow}, ret = parseSyntaxType()]:
                                        TFunction([new FunctionArg("", t)], ret);
                                });

                            case [{kind: TkComma}]:
                                switch stream {
                                    case [{kind: TkParenClose}]:
                                        if (peek(0).kind == TkArrow)
                                            unexpected();
                                        TTuple([t]);

                                    case [types = separated(TkComma, parseSyntaxType), {kind: TkParenClose}]:
                                        types.unshift(t);

                                        switch stream {
                                            case [{kind: TkArrow}, ret = parseSyntaxType()]:
                                                TFunction([for (t in types) new FunctionArg("", t)], ret);
                                            case _:
                                                TTuple(types);
                                        }
                                }
                        }
                }

            case [path = separated(TkDot, parseIdent)]:
                if (path.length == 0)
                    unexpected();
                var name = path.pop();
                TPath(path, name);
        }
    }

    function parseIdent():String {
        return switch stream {
            case [{kind: TkIdent(ident)}]:
                ident;
        }
    }

    function parseExprWithSemicolon():Expr {
        return switch stream {
            case [e = parseExpr(), _ = checkSemicolon()]:
                e;
        }
    }

    function parseExpr():Expr {
        return switch stream {
            // literal ("hi", 1, 1.5, etc.) is easy, nothing to do here
            case [{kind: TkLiteral(literal)}]:
                parseExprNext(mk(ELiteral(literal), last.pos));

            // block, i.e. a list of expressions between {}, separated by semicolons
            case [{kind: TkBraceOpen, pos: pmin}, exprs = parseRepeat(parseExprWithSemicolon), {kind: TkBraceClose}]:
                mk(EBlock(exprs), Position.union(pmin, last.pos));

            // identifier can be either simple identifier expr, or an argument name for the short lambda
            case [{kind: TkIdent(ident), pos: pmin}]:
                switch stream {
                    // simpliest one argument arrow function, e.g. `x => ...`
                    case [{kind: TkArrow}, e = parseExpr()]:
                        mk(EArrowFunction([new FunctionArg(ident, null)], null, e), Position.union(pmin, last.pos));

                    // if there was no arrow, it's a simple identifier expression, e.g. `x`
                    case _:
                        parseExprNext(mk(EIdent(ident), last.pos));
                }

            // opening paren can lead to different things: expr in parens, a tuple or start of a short lambda
            case [{kind: TkParenOpen, pos: pmin}]:
                switch stream {
                    // closing paren just after opening is either an empty tuple or no-argument short lambda
                    case [{kind: TkParenClose}]:
                        switch stream {
                            // if there's an arrow - it's an arrow function, expect and expression
                            case [{kind: TkArrow}]:
                                var e = parseExpect(parseExpr);
                                mk(EArrowFunction([], null, e), Position.union(pmin, last.pos));

                            // if there's a type hint after this - it's a no-argument short lambda
                            // with explicit return type declaration
                            case [t = parseTypeHint()]:
                                // obviously, we expect an arrow followed by an expression here
                                parseExpect(function() return switch stream {
                                    case [{kind: TkArrow}, e = parseExpr()]:
                                        mk(EArrowFunction([], t, e), Position.union(pmin, last.pos));
                                });

                            // otherwise, it's an empty tuple
                            case _:
                                parseExprNext(mk(ETuple([]), Position.union(pmin, last.pos)));
                        }

                    case [e = parseExpr()]:
                        switch stream {
                            case [{kind: TkParenClose}]:
                                parseExprNext(mk(EParens(e), Position.union(pmin, last.pos)));
                            case [{kind: TkComma}, exprs = separated(TkComma, parseExpr), {kind: TkParenClose}]:
                                exprs.unshift(e);
                                parseExprNext(mk(ETuple(exprs), Position.union(pmin, last.pos)));
                        }
                }

            case [{kind: TkKeyword(KwdIf), pos: pmin}, {kind: TkParenOpen}, econd = parseExpr(), {kind: TkParenClose}, ethen = parseExpr()]:
                var eelse = switch stream {
                    case [{kind: TkKeyword(KwdElse)}, e = parseExpr()]:
                        e;
                    case _:
                        switch [peek(0), peek(1)] {
                            case [{kind: TkSemicolon}, {kind: TkKeyword(KwdElse)}]:
                                junk();
                                junk();
                                parseExpr();
                            case _:
                                null;
                        }
                }
                mk(EIf(econd, ethen, eelse), Position.union(pmin, last.pos));

            case [v = parseVar()]:
                mk(EVar(v.name, v.type, v.initial), v.pos);
        }
    }

    function parseExprNext(expr:Expr):Expr {
        return switch stream {
            case [{kind: TkParenOpen}, args = separated(TkComma, parseExpr), {kind: TkParenClose}]:
                parseExprNext(mk(ECall(expr, args), Position.union(expr.pos, last.pos)));
            case [{kind: TkDot}, {kind: TkIdent(ident)}]:
                parseExprNext(mk(EField(expr, ident), Position.union(expr.pos, last.pos)));
            case _:
                expr;
        }
    }

    function checkSemicolon():Void {
        if (last.kind == TkBraceClose) {
            switch stream {
                case [{kind: TkSemicolon}]:
                case _:
            }
        } else {
            switch stream {
                case [{kind: TkSemicolon}]:
                case _:
                    throw new ParserError(MissingSemicolon, new Position(last.pos.file, last.pos.max, last.pos.max));
            }
        }
    }

    function separated<T>(kind:TokenKind, f:Void->T):Array<T> {
        return parseSeparated(function(t) return t.kind == kind, f);
    }

    inline function mk(kind:ExprKind, pos:Position):Expr {
        return new Expr(kind, pos);
    }
}
