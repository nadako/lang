import js.Browser.document;
import js.html.Element;
import js.html.TextAreaElement;

@:expose
class Web {
    static var code:TextAreaElement = cast document.getElementById("code");
    static var result:Element = cast document.getElementById("result");

    static function main(editor) {
        var model:{function getValue():String; function onDidChangeContent(f:Void->Void):Void;} = editor.getModel();
        function compile() {
            try {
                var input = byte.ByteData.ofString(model.getValue());
                var parser = new toylang.Parser(input, "code");
                var typer = new toylang.Typer();

                var decls = parser.parse();
                var r = [];
                for (decl in decls) {
                    var typedDecl = typer.typeDecl(decl);
                    r.push(toylang.Dump.dumpTypeDecl(typedDecl));
                }

                result.innerText = r.join("\n\n");
            } catch (e:Any) {
                result.innerText = 'ERROR: $e';
            }
        }
        model.onDidChangeContent(compile);
        compile();
    }
}
