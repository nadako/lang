package toylang;

import Sys.println;
import sys.FileSystem;
import sys.io.File;

class Main {
    static function loadModule(classPaths:Array<String>, name:String) {
        for (cp in classPaths) {
            var fullPath = cp + "/" + name + ".toy";
            if (FileSystem.exists(fullPath)) {
                var source = File.getContent(fullPath);
                var parser = new Parser(source, fullPath, [name]);
                var syntaxDecls = parser.parse();
                println('Module $name ($fullPath)');
                println('Declarations:');
                var printer = new Printer("  ");
                for (decl in syntaxDecls) {
                    println(printer.printSyntaxDecl(decl));
                }
                return;
            }
        }
        throw 'Module $name was not found in any of class paths (${classPaths.join(", ")})';
    }

    static function main() {
        // var cps = ["toys"];
        // cps = cps.map(FileSystem.fullPath);
        // loadModule(cps, "main");
        var file = "main.toy";
        var source = sys.io.File.getContent(file);
        var input = byte.ByteData.ofString(source);

        var parser = new Parser(source, file, ["main"]);
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
                decls.map(typer.typeSyntaxDecl);
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

        var genjs = new GenJs();
        var jsCode = genjs.generate(typedDecls);
        sys.io.File.saveContent('out.js', jsCode);

        // var gencs = new GenCs();
        // var csCode = gencs.generate(decls);
        // sys.io.File.saveContent('out.cs', csCode);
        // Sys.command("C:/Program Files (x86)/Unity/Editor/Data/Mono/bin/gmcs.bat", ["out.cs", "-target:library"]);

        for (decl in typedDecls) {
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
