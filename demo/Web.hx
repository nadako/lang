import js.Browser.document;
import js.html.Element;
import js.html.DivElement;
import js.html.TextAreaElement;

@:native("vis.DataSet")
extern class VisDataSet {
    function new(contents:Array<{}>);
}


typedef VisNetworkData = {nodes:VisDataSet, edges:VisDataSet}

@:native("vis.Network")
extern class VisNetwork {
    function new(element:DivElement, data:VisNetworkData, options:{});
    function setData(data:VisNetworkData):Void;
    function destroy():Void;
}

@:expose
class Web {
    static var code:TextAreaElement = cast document.getElementById("code");
    static var result:TextAreaElement = cast document.getElementById("result");
    static var error:Element = cast document.getElementById("error");
    static var cfg:DivElement = cast document.getElementById("cfg");
    static var network:VisNetwork;

    static function main(editor) {
        var model:{function getValue():String; function onDidChangeContent(f:Void->Void):Void;} = editor.getModel();
        function compile() {
            var firstFun = null;
            var typer = new toylang.Typer();
            var success = try {
                var parser = new toylang.Parser(model.getValue(), "code", ["code"]);

                var decls = parser.parse();
                var r = [];
                var typed = [];
                for (decl in decls) {
                    var typedDecl = typer.typeDecl(decl);
                    typed.push(typedDecl);
                    if (firstFun == null) {
                        switch (typedDecl) {
                            case TDFunction(fun) if (fun.cfg != null):
                                firstFun = fun;
                            case _:
                        }
                    }
                }

                var gen = new toylang.GenJs();
                var code = gen.generate(typed);

                result.innerText = code;
                error.innerText = "";
                true;
            } catch (e:Any) {
                error.innerText = 'ERROR: $e';
                result.innerText = "";
                false;
            }

            if (!success || firstFun == null) {
                if (network != null) {
                    network.destroy();
                    network = null;
                }
                return;
            }

            var data = toylang.DebugUtils.makeVisJsGraph(firstFun.cfg);
            trace(data);
            var data = {
                nodes: new VisDataSet(data.nodes),
                edges: new VisDataSet(data.edges),
            };
            if (network != null)
                network.setData(data);
            else {
                var options = {
                    layout: {
                        hierarchical: {
                            enabled: true,
                            sortMethod: "directed"
                        }
                    },
                    edges: {
                        smooth: {type: "continuous"}
                    },
                    physics: {
                        enabled: false
                    }
                };
                network = new VisNetwork(cfg, data, options);
            }
        }
        model.onDidChangeContent(compile);
        compile();
    }
}
