package toylang;


class Main {
    static function main() {
        var file = "main.toy";
        var input = byte.ByteData.ofBytes(sys.io.File.getBytes(file));
        var parser = new Parser(input, file);
        var printer = new Printer();
        var typer = new Typer();

        var decls =
            try {
                parser.parse();
            } catch (e:hxparse.ParserError) {
                Sys.println(e.pos.format(input) + ": " + Std.string(e.toString()));
                Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
                return;
            } catch (e:toylang.Parser.ParserError) {
                Sys.println(e.pos.format(input) + ": " + Std.string(e.message));
                Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
                return;
            };

        // for (decl in decls)
        //     Sys.println(printer.printDecl(decl));

        var typedDecls =
            try {
                decls.map(typer.typeDecl);
            } catch(e:toylang.Typer.TyperError) {
                Sys.print(e.pos.format(input) + ": ");
                switch (e.message) {
                    case UnificationError(actual, expected):
                        Sys.println('`${Dump.typeToString(actual)}` should be `${Dump.typeToString(expected)}`');
                    case other:
                        Sys.println(Std.string(other));
                }
                Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
                return;
            }

        for (decl in decls) {
            var typed = typer.typeDecl(decl);
            Sys.println(Dump.dumpTypeDecl(typed));
        }
    }
}
