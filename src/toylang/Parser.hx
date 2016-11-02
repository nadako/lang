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
                var field = new Field();
                field.name = f.fun.name;
                field.pos = f.pos;
                field.kind = FFun(f.fun);
                field;

            case [v = parseVar(), _ = checkSemicolon()]:
                var field = new Field();
                field.name = v.name;
                field.pos = v.pos;
                field.kind = FVar(v.type, v.initial);
                field;
        }
    }

    function parseFunction():{fun:FunctionDecl, pos:Position} {
        return switch stream {
            case [{kind: TkKeyword(KwdFunction), pos: pmin}, {kind: TkIdent(name)}, {kind: TkParenOpen}, args = separated(TkComma, parseFunctionArg), {kind: TkParenClose}, ret = parseOptional(parseTypeHint)]:
                var expr = switch stream {
                    case [e = parseExprWithSemicolon()]:
                        e;
                    case [{kind: TkSemicolon}]:
                        null;
                    case _:
                        unexpected();
                };
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
                var arg = new FunctionArg();
                arg.name = name;
                arg.type = type;
                arg;
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
                        TTuple([]);
                    case [t = parseSyntaxType()]:
                        switch stream {
                            case [{kind: TkParenClose}]:
                                t;
                            case [{kind: TkComma}]:
                                switch stream {
                                    case [{kind: TkParenClose}]:
                                        TTuple([t]);
                                    case [types = separated(TkComma, parseSyntaxType), {kind: TkParenClose}]:
                                        types.unshift(t);
                                        TTuple(types);
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
            case [{kind: TkBraceOpen, pos: pmin}, exprs = parseRepeat(parseExprWithSemicolon), {kind: TkBraceClose}]:
                mk(EBlock(exprs), Position.union(pmin, last.pos));
            case [{kind: TkIdent(ident)}]:
                parseExprNext(mk(EIdent(ident), last.pos));
            case [{kind: TkLiteral(literal)}]:
                parseExprNext(mk(ELiteral(literal), last.pos));
            case [{kind: TkParenOpen, pos: pmin}]:
                switch stream {
                    case [{kind: TkParenClose}]:
                        parseExprNext(mk(ETuple([]), Position.union(pmin, last.pos)));
                    case [e = parseExpr()]:
                        switch stream {
                            case [{kind: TkParenClose}]:
                                parseExprNext(mk(EParens(e), Position.union(pmin, last.pos)));
                            case [{kind: TkComma}, exprs = separated(TkComma, parseExpr), {kind: TkParenClose}]:
                                exprs.unshift(e);
                                parseExprNext(mk(ETuple(exprs), Position.union(pmin, last.pos)));
                        }
                }
            case [{kind: TkKeyword(KwdIf), pos: pmin}, econd = parseExpr(), ethen = parseExpr()]:
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
