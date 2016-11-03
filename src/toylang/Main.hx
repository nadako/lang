package toylang;


class Main {
    static function main() {
        var file = "main.toy";
        var input = byte.ByteData.ofBytes(sys.io.File.getBytes(file));
        var parser = new Parser(input, file);
        var printer = new Printer();
        var typer = new Typer();

        try {
            var decls = parser.parse();
            for (decl in decls) {
                // Sys.println(printer.printDecl(decl));
                var typed = typer.typeDecl(decl);
                Sys.println(Dump.dumpTypeDecl(typed));
            }
        } catch(e:toylang.Typer.TyperError) {
            Sys.print(e.pos.format(input) + ": ");
            switch (e.message) {
                case UnificationError(actual, expected):
                    Sys.println('`${typeToString(actual)}` should be `${typeToString(expected)}`');
                case other:
                    Sys.println(Std.string(other));
            }
            Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
        } catch (e:hxparse.ParserError) {
            Sys.println(e.pos.format(input) + ": " + Std.string(e.toString()));
            Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
        } catch (e:toylang.Parser.ParserError) {
            Sys.println(e.pos.format(input) + ": " + Std.string(e.message));
            Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
        }
    }

    static function typeToString(t:Type):String {
        return switch (t) {
            case TMono(m):
                if (m.type == null)
                    "<unknown>";
                else
                    typeToString(m.type);
            case TInst(c):
                c.module.concat([c.name]).join(".");
            case TTuple(types):
                var b = new StringBuf();
                b.add("(");
                b.add(types.map(typeToString).join(", "));
                if (types.length == 1)
                    b.add(",");
                b.add(")");
                b.toString();
            case TFun(args, ret):
                var b = new StringBuf();
                b.add("(");
                b.add([for (a in args) typeToString(a.type)].join(", "));
                b.add(") => ");
                b.add(typeToString(ret));
                b.toString();
        }
    }
}
