package toylang;

import toylang.Syntax;

class LexerError {
    public var message:LexerErrorMessage;
    public var pos:Position;

    public function new(message, pos) {
        this.message = message;
        this.pos = pos;
    }

    public function toString():String {
        return '$pos: $message';
    }
}

enum LexerErrorMessage {
    UnclosedString;
    UnknownEscapeSequence(s:String);
}

class Lexer extends hxparse.Lexer implements hxparse.RuleBuilder {
    static var keywords = @:mapping(3) Keyword;
    static var buf:StringBuf;

    inline static var whitespace = "[\r\n\t ]+";
    inline static var integer = "([1-9][0-9]*)|0";
    inline static var commentLine = "//[^\r\n]*";
    inline static var ident = "(_|[a-zA-Z])+[a-zA-Z0-9_]*";

    inline static function mkPos(pos:hxparse.Position):Position {
        return new Position(pos.psource, pos.pmin, pos.pmax);
    }

    inline static function mk(lexer:hxparse.Lexer, kind:TokenKind):Token {
        return new Token(kind, mkPos(lexer.curPos()));
    }

    public static var rule = @:rule [
        "" => mk(lexer, TkEof),
        whitespace => lexer.token(rule),
        integer => mk(lexer, TkLiteral(LInt(lexer.current))),
        commentLine => lexer.token(rule),
        "," => mk(lexer, TkComma),
        "\\." => mk(lexer, TkDot),
        ";" => mk(lexer, TkSemicolon),
        ":" => mk(lexer, TkColon),
        "{" => mk(lexer, TkBraceOpen),
        "}" => mk(lexer, TkBraceClose),
        "=>" => mk(lexer, TkArrow),
        "=" => mk(lexer, TkEqual),
        "\\(" => mk(lexer, TkParenOpen),
        "\\)" => mk(lexer, TkParenClose),
        "\"" => {
            buf = new StringBuf();
            var pmin = lexer.curPos();
            try lexer.token(string) catch (_:haxe.io.Eof) throw new LexerError(UnclosedString, new Position(pmin.psource, pmin.pmin, lexer.curPos().pmax - 1));
            var pmax = lexer.curPos();
            var kind = TkLiteral(LString(unescapeString(buf.toString(), new Position(pmin.psource, pmin.pmin, pmin.pmin))));
            buf = null;
            new Token(kind, new Position(pmin.psource, pmin.pmin, pmax.pmax));
        },
        ident => {
            var kwd = keywords[lexer.current];
            if (kwd != null)
                mk(lexer, TkKeyword(kwd));
            else
                mk(lexer, TkIdent(lexer.current));
        }
    ];

    static var string = @:rule [
        "\\\\\\\\" => {
            buf.add("\\\\");
            lexer.token(string);
        },
        "\\\\" => {
            buf.add("\\");
            lexer.token(string);
        },
        "\\\\\"" => {
            buf.add("\"");
            lexer.token(string);
        },
        "\"" => {},
        "[^\\\\\"]+" => {
            buf.add(lexer.current);
            lexer.token(string);
        }
    ];

    static function unescapeString(s:String, pos:Position):String {
        var buf = new StringBuf();
        var i = 0;
        var inEscape = false;
        while (i < s.length) {
            var c = s.charCodeAt(i);
            if (!inEscape) {
                switch (c) {
                    case "\\".code:
                        inEscape = true;
                    default:
                        buf.addChar(c);
                }
            } else {
                switch (c) {
                    case "n".code:
                        buf.add("\n");
                    case "r".code:
                        buf.add("\r");
                    case "t".code:
                        buf.add("\t");
                    case "\"".code | "'".code | "\\".code:
                        buf.addChar(c);
                    default:
                        throw new LexerError(UnknownEscapeSequence("\\" + String.fromCharCode(c)), new Position(pos.file, pos.min + i, pos.min + i + 2));
                }
                inEscape = false;
            }
            i++;
        }
        return buf.toString();
    }

    public static function escapeString(s:String):String {
        var buf = new StringBuf();
        for (i in 0...s.length) {
            switch (s.charCodeAt(i)) {
                case "\n".code:
                    buf.add("\\n");
                case "\r".code:
                    buf.add("\\r");
                case "\t".code:
                    buf.add("\\t");
                case "\"".code:
                    buf.add("\\\"");
                case "\\".code:
                    buf.add("\\\\");
                case other:
                    buf.addChar(other);
            }
        }
        return buf.toString();
    }
}
