package toylang;

import toylang.Syntax;
using StringTools;

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

            case [{kind: TkKeyword(KwdClass), pos: pmin}, {kind: TkIdent(name)}, params = parseTypeParams(), {kind: TkBraceOpen}, fields = parseRepeat(parseClassField), {kind: TkBraceClose}]:
                var decl = new Decl();
                decl.name = name;
                decl.pos = Position.union(pmin, last.pos);
                decl.kind = DClass({
                    var cls = new ClassDecl();
                    cls.fields = fields;
                    cls.params = params;
                    cls;
                });
                decl;
        }
    }

    function parseTypeParams():Array<TypeParamDecl> {
        return switch stream {
            case [{kind: TkLt}, params = separated(TkComma, parseIdent), {kind: TkGt}]:
                [for (p in params) new TypeParamDecl(p)];
            case _: [];
        }
    }

    function parseClassField():Field {
        return switch stream {
            case [{kind: TkKeyword(KwdConst)}, f = parseFunction()]:
                var f = new Field(f.fun.name, FFun(f.fun), f.pos);
                f.modifiers.push(FMConst);
                return f;

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
                    case [{kind: TkEquals}, e = parseExpr()]: e;
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

            case [{kind: TkKeyword(KwdConst)}, {kind: TkParenOpen}, type = parseSyntaxType(), {kind: TkParenClose}]:
                TConst(type);

            // otherwise try parsing simple dot path
            case [path = parseTypePath()]:
                TPath(path);
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
                        new FunctionArg("", TPath(new TypePath(path, name)));

                    // otherwise - it's a toplevel type path (e.g. Int)
                    case _:
                        new FunctionArg("", TPath(new TypePath([], ident)));
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

    function parseTypePath():TypePath {
        var path = parseDotPath([]);
        var name = path.pop();
        return new TypePath(path, name);
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

            case [{kind: TkKeyword(KwdThis), pos: pmin}]:
                parseExprNext(mk(EIdent("this"), last.pos));

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

            case [{kind: TkKeyword(KwdNew), pos: pmin}, path = parseExpect(parseTypePath)]:
                mk(ENew(path), Position.union(pmin, last.pos));

            case [{kind: TkBang, pos: pmin}, expr = parseExpect(parseExpr)]:
                mPrefixUnop(OpNot, expr, pmin);

            case [{kind: TkMinus, pos: pmin}, expr = parseExpect(parseExpr)]:
                mPrefixUnop(OpNeg, expr, pmin);

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

            case [{kind: TkEquals}, right = parseExpect(parseExpr)]:
                mk(EAssign(expr, right), Position.union(expr.pos, last.pos));

            case [{kind: TkEqualsEquals}, right = parseExpect(parseExpr)]:
                mkBinop(OpEq, expr, right);

            case [{kind: TkBangEquals}, right = parseExpect(parseExpr)]:
                mkBinop(OpNotEq, expr, right);

            case [{kind: TkPlus}, right = parseExpect(parseExpr)]:
                mkBinop(OpAdd, expr, right);

            case [{kind: TkMinus}, right = parseExpect(parseExpr)]:
                mkBinop(OpSub, expr, right);

            case [{kind: TkSlash}, right = parseExpect(parseExpr)]:
                mkBinop(OpDiv, expr, right);

            case [{kind: TkAsterisk}, right = parseExpect(parseExpr)]:
                mkBinop(OpMul, expr, right);

            case [{kind: TkLt}, right = parseExpect(parseExpr)]:
                mkBinop(OpLt, expr, right);

            case [{kind: TkLte}, right = parseExpect(parseExpr)]:
                mkBinop(OpLte, expr, right);

            case [{kind: TkGt}, right = parseExpect(parseExpr)]:
                mkBinop(OpGt, expr, right);

            case [{kind: TkGte}, right = parseExpect(parseExpr)]:
                mkBinop(OpGte, expr, right);

            case [{kind: TkAmpAmp}, right = parseExpect(parseExpr)]:
                mkBinop(OpBoolAnd, expr, right);

            case [{kind: TkPipePipe}, right = parseExpect(parseExpr)]:
                mkBinop(OpBoolOr, expr, right);

            case [{kind: TkBang}]:
                mk(EUnop(OpNot, expr, true), Position.union(expr.pos, last.pos));

            case _:
                expr;
        }
    }

    function mPrefixUnop(op:Unop, expr:Expr, pmin:Position):Expr {
        return switch (expr.kind) {
            case ELiteral(LInt(i)):
                i = if (i.fastCodeAt(0) == "-".code) i.substring(1) else "-" + i;
                mk(ELiteral(LInt(i)), Position.union(pmin, expr.pos));

            case EBinop(binop, left, right):
                var left = mk(EUnop(op, left, false), Position.union(pmin, left.pos));
                mk(EBinop(binop, left, right), Position.union(pmin, right.pos));
            case _:
                mk(EUnop(op, expr, false), Position.union(pmin, expr.pos));
        }
    }

    function mkBinop(op:Binop, left:Expr, right:Expr):Expr {
        return switch (right.kind) {
            case EBinop(rOp, rLeft, rRight) if (precedence(op) <= precedence(rOp)):
                var left = mkBinop(op, left, rLeft);
                return mk(EBinop(rOp, left, rRight), Position.union(left.pos, rRight.pos));
            case EAssign(rLeft, rRight):
                var left = mkBinop(op, left, rLeft);
                return mk(EAssign(left, rRight), Position.union(left.pos, rRight.pos));
            case _:
                return mk(EBinop(op, left, right), Position.union(left.pos, right.pos));
        }
    }

    static function precedence(op:Binop):Int {
        return switch (op) {
            case OpMul | OpDiv : 1;
            case OpAdd | OpSub : 2;
            case OpGt | OpLt | OpGte | OpLte : 3;
            case OpEq | OpNotEq: 4;
            case OpBoolAnd : 5;
            case OpBoolOr : 6;
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
