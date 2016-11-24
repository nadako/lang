package toylang;

import toylang.Syntax;

using StringTools;

class Scanner {
    var text:String;
    var file:String;
    var tokenPos:Int;
    var pos:Int;
    var end:Int;

    static var keywords = [for (kwd in Keyword.createAll()) kwd.getName().substring(3).toLowerCase() => kwd];

    public static function escapeString(s:String):String {
        var result = "";
        for (i in 0...s.length) {
            switch (s.fastCodeAt(i)) {
                case "\n".code:
                    result += "\\n";
                case "\r".code:
                    result += "\\r";
                case "\t".code:
                    result += "\\t";
                case "\"".code:
                    result += "\\\"";
                case "\\".code:
                    result += "\\\\";
                case other:
                    result += String.fromCharCode(other);
            }
        }
        return result;
    }

    public function new(text, file) {
        this.text = text;
        this.file = file;
        this.pos = 0;
        this.end = text.length;
    }

    public function reportError(msg) {
        // this can be pluggable for diagnostics
        trace('$msg at $file ofs ${pos - 1}');
    }

    public function scan():Token {
        while (true) {
            tokenPos = pos;
            if (pos >= end)
                return mk(TkEof);

            var ch = text.fastCodeAt(pos);
            switch (ch) {
                case "\r".code | "\n".code | "\t".code | " ".code:
                    pos++;
                    continue;

                case "!".code:
                    pos++;
                    if (text.fastCodeAt(pos) == "=".code) {
                        pos++;
                        return mk(TkBangEquals);
                    }
                    return mk(TkBang);

                case "=".code:
                    pos++;
                    var ch = text.fastCodeAt(pos);
                    if (ch == "=".code) {
                        pos++;
                        return mk(TkEqualsEquals);
                    } else if (ch == ">".code) {
                        pos++;
                        return mk(TkArrow);
                    }
                    return mk(TkEquals);

                case "&".code:
                    pos++;
                    if (text.fastCodeAt(pos) == "&".code) {
                        pos++;
                        return mk(TkAmpAmp);
                    }
                    reportError("Invalid character " + String.fromCharCode(ch));
                    return mk(TkInvalid);

                case "|".code:
                    pos++;
                    if (text.fastCodeAt(pos) == "|".code) {
                        pos++;
                        return mk(TkPipePipe);
                    }
                    reportError("Invalid character " + String.fromCharCode(ch));
                    return mk(TkInvalid);

                case "\\".code:
                    pos++;
                    return mk(TkBackslash);

                case "(".code:
                    pos++;
                    return mk(TkParenOpen);

                case ")".code:
                    pos++;
                    return mk(TkParenClose);

                case "{".code:
                    pos++;
                    return mk(TkBraceOpen);

                case "}".code:
                    pos++;
                    return mk(TkBraceClose);

                case "<".code:
                    pos++;
                    if (text.fastCodeAt(pos) == "=".code) {
                        pos++;
                        return mk(TkLte);
                    }
                    return mk(TkLt);

                case ">".code:
                    pos++;
                    if (text.fastCodeAt(pos) == "=".code) {
                        pos++;
                        return mk(TkGte);
                    }
                    return mk(TkGt);

                case "+".code:
                    pos++;
                    return mk(TkPlus);

                case "-".code:
                    pos++;
                    return mk(TkMinus);

                case "*".code:
                    pos++;
                    return mk(TkAsterisk);

                case "/".code:
                    pos++;
                    return mk(TkSlash);

                case ",".code:
                    pos++;
                    return mk(TkComma);

                case ".".code:
                    pos++;
                    return mk(TkDot);

                case ":".code:
                    pos++;
                    return mk(TkColon);

                case ";".code:
                    pos++;
                    return mk(TkSemicolon);

                case "\"".code:
                    pos++;
                    var s = scanString();
                    return mk(TkLiteral(LString(s)));

                case "1".code | "2".code | "3".code | "4".code | "5".code | "6".code | "7".code | "8".code | "9".code:
                    pos++;
                    while (pos < end) {
                        var ch = text.fastCodeAt(pos);
                        if (!isNumeric(ch))
                            break;
                        pos++;
                    }
                    return mk(TkLiteral(LInt(text.substring(tokenPos, pos))));

                case _ if (isIdentStart(ch)):
                    pos++;
                    while (pos < end) {
                        var ch = text.fastCodeAt(pos);
                        if (!isIdentPart(ch))
                            break;
                        pos++;
                    }
                    return mkIdentOrKeyword();

                default:
                    pos++;
                    reportError("Invalid character " + String.fromCharCode(ch));
                    return mk(TkInvalid);
            }
        }
    }

    function scanString():String {
        var result = "";
        var start = pos;
        while (true) {
            if (pos >= end) {
                result += text.substring(start, pos);
                reportError("Unterminated string");
                break;
            }
            // not using switch because of https://github.com/HaxeFoundation/haxe/pull/4964
            var ch = text.fastCodeAt(pos);
            if (ch == "\"".code) {
                result += text.substring(start, pos);
                pos++;
                break;
            } else if (ch == "\\".code) {
                result += text.substring(start, pos);
                pos++;
                result += scanEscapeSequence();
                start = pos;
            } else {
                pos++;
            }
        }
        return result;
    }

    function scanEscapeSequence():String {
        if (pos >= end) {
            reportError("Unterminated escape sequence");
            return "";
        }
        var ch = text.fastCodeAt(pos);
        pos++;
        return switch (ch) {
            case "t".code:
                "\t";
            case "n".code:
                "\n";
            case "r".code:
                "\r";
            case "\"".code:
                "\"";
            default:
                reportError("Invalid escape sequence");
                "";
        }
    }

    function mkIdentOrKeyword() {
        var ident = text.substring(tokenPos, pos);
        var kwd = keywords[ident];
        return if (kwd != null) mk(TkKeyword(kwd)) else mk(TkIdent(ident));
    }

    inline static function isIdentStart(ch:Int):Bool {
        return ch == "_".code || (ch >= "a".code && ch <= "z".code) || (ch >= "A".code && ch <= "Z".code);
    }

    inline static function isIdentPart(ch:Int):Bool {
        return isIdentStart(ch) || isNumeric(ch);
    }

    inline static function isNumeric(ch:Int):Bool {
        return (ch >= "0".code && ch <= "9".code);
    }

    inline function mk(kind) return new Token(kind, new Position(file, tokenPos, pos));
}
