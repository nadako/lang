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
            case [{kind: TkColon}, type = parseExpect(parseSyntaxType)]:
                type;
        }
    }

    function parseSyntaxType():SyntaxType {
        return switch stream {
            // backslash means "lambda", so it's a function type
            case [{kind: TkBackslash}]:
                switch stream {
                    case [{kind: TkParenOpen}, args = separated(TkComma, parseLambdaArg), {kind: TkParenClose}, {kind: TkArrow}, ret = parseSyntaxType()]:
                        TFunction(args, ret);
                    case _:
                        unexpected();
                }

            // opening paren means it's a tuple
            case [{kind: TkParenOpen}]:
                switch stream {
                    case [{kind: TkParenClose}]:
                        TTuple([]);
                    case [type = parseSyntaxType(), {kind: TkComma}, types = separated(TkComma, parseSyntaxType), {kind: TkParenClose}]:
                        types.unshift(type);
                        TTuple(types);
                    case _:
                        unexpected();
                }

            // otherwise try parsing simple dot path
            case [path = parseDotPath([])]:
                var name = path.pop();
                TPath(path, name);
        }
    }

    function parseLambdaArg():FunctionArg {
        // lambda arg can be either Type, or name:Type
        return switch stream {
            // so when we encounter an identifier - it could be either a start of dot-path or an argument name
            case [{kind: TkIdent(ident)}]:
                switch stream {
                    // if there's a colon - it's clearly a name - parse type after the colon
                    case [{kind: TkColon}, type = parseExpect(parseSyntaxType)]:
                        new FunctionArg(ident, type);

                    // if there's a dot - it's a type path
                    case [{kind: TkDot}, path = parseDotPath([ident])]:
                        var name = path.pop();
                        new FunctionArg("", TPath(path, name));

                    // otherwise - it's a toplevel type path (e.g. Int)
                    case _:
                        new FunctionArg("", TPath([], ident));
                }

            // if there was no identifier - try parsing a type
            case _:
                new FunctionArg("", parseSyntaxType());
        }
    }

    function parseDotPath(acc:Array<String>):Array<String> {
        while (true) {
            switch stream {
                case [{kind: TkIdent(ident)}]:
                    acc.push(ident);
                    switch stream {
                        case [{kind: TkDot}]:
                        case _:
                            break;
                    }
            }
        }
        return acc;
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

            // identifier is, well, an identifier :)
            case [{kind: TkIdent(ident), pos: pmin}]:
                parseExprNext(mk(EIdent(ident), last.pos));

            // opening paren can lead to different things: single expr in parens or a tuple
            case [{kind: TkParenOpen, pos: pmin}]:
                switch stream {
                    // closing paren just after opening is an empty tuple
                    case [{kind: TkParenClose}]:
                        parseExprNext(mk(ETuple([]), Position.union(pmin, last.pos)));

                    // if there was an expression - it's either expr in parens or a tuple, depending on whether there's a comma
                    case [e = parseExpr()]:
                        switch stream {
                            // closing paren - it's a simple expr
                            case [{kind: TkParenClose}]:
                                parseExprNext(mk(EParens(e), Position.union(pmin, last.pos)));

                            // comma - it's a tuple!
                            case [{kind: TkComma}, exprs = separated(TkComma, parseExpr), {kind: TkParenClose}]:
                                exprs.unshift(e);
                                parseExprNext(mk(ETuple(exprs), Position.union(pmin, last.pos)));
                        }
                }

            // backslash means "lambda", so it's a function expr
            case [{kind: TkBackslash, pos: pmin}]:
                switch stream {
                    // simple \x => x case
                    case [{kind: TkIdent(arg)}]:
                        switch stream {
                            case [{kind: TkArrow}, e = parseExpr()]:
                                mk(EArrowFunction([new FunctionArg(arg, null)], null, e), Position.union(pmin, last.pos));
                            case _:
                                unexpected();
                        }

                    // all other cases require parens
                    case [{kind: TkParenOpen}, args = separated(TkComma, parseFunctionArg), {kind: TkParenClose}, ret = parseOptional(parseTypeHint), {kind: TkArrow}, e = parseExpr()]:
                        mk(EArrowFunction(args, ret, e), Position.union(pmin, last.pos));

                    case _:
                        unexpected();
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

            case [{kind: TkKeyword(KwdWhile), pos: pmin}, {kind: TkParenOpen}, econd = parseExpr(), {kind: TkParenClose}, ebody = parseExpr()]:
                mk(EWhile(econd, ebody), Position.union(pmin, last.pos));

            case [{kind: TkKeyword(KwdBreak)}]:
                mk(EBreak, last.pos);

            case [{kind: TkKeyword(KwdContinue)}]:
                mk(EContinue, last.pos);

            case [{kind: TkKeyword(KwdReturn), pos: pmin}, e = parseOptional(parseExpr)]:
                mk(EReturn(e), Position.union(pmin, last.pos));

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
        var acc = [];
        var hadSep = false;
        while(true) {
            try {
                acc.push(f());
            } catch(e:hxparse.NoMatch<Dynamic>) {
                if (hadSep) // forbid trailing separator
                    unexpected();
                break;
            }
            if (peek(0).kind == kind) {
                hadSep = true;
                junk();
            } else {
                break;
            }
        }
        return acc;
    }

    inline function mk(kind:ExprKind, pos:Position):Expr {
        return new Expr(kind, pos);
    }
}
