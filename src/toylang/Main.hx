package toylang;

class Main {
    static function main() {
        var file = "main.toy";
        var input = byte.ByteData.ofBytes(sys.io.File.getBytes(file));

        var parser = new Parser(input, file);
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

        // var printer = new Printer();
        // for (decl in decls)
        //     Sys.println(printer.printDecl(decl));

        var typer = new Typer();
        var typedDecls =
            try {
                decls.map(typer.typeDecl);
            } catch(e:toylang.Typer.TyperError) {
                Sys.print(e.pos.format(input) + ": ");
                switch (e.message) {
                    case UnificationError(actual, expected):
                        Sys.println('`${DebugUtils.typeToString(actual)}` should be `${DebugUtils.typeToString(expected)}`');
                    case other:
                        Sys.println(Std.string(other));
                }
                Sys.println(haxe.CallStack.toString(haxe.CallStack.exceptionStack()));
                return;
            }

        var decls = decls.map(typer.typeDecl);

        var genjs = new GenJs();
        var jsCode = genjs.generate(decls);
        sys.io.File.saveContent('out.js', jsCode);

        for (decl in decls) {
            switch (decl) {
                case TDFunction(fun) if (fun.cfg != null):
                    var graph = DebugUtils.makeDotGraph(fun.cfg);

                    var name = 'graph-${fun.name}';
                    sys.io.File.saveContent('$name.dot', graph);
                    Sys.command("C:/Program Files (x86)/Graphviz2.38/bin/dot.exe", ['$name.dot', '-o$name.png', "-Tpng"]);
                case _:
            }
        }
    }
}
