package toylang;


class Main {
    static function main() {
        var file = "main.toy";
        var input = byte.ByteData.ofBytes(sys.io.File.getBytes(file));
        var parser = new Parser(input, file);
        var printer = new Printer();
        var decls = parser.parse();
        var typer = new Typer();
        for (decl in decls) {
            Sys.println(printer.printDecl(decl));
            var typed = typer.typeDecl(decl);
            Sys.println(Dump.dumpTypeDecl(typed));
        }
    }

}
