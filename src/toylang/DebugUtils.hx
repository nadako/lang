package toylang;

import toylang.Type;

class DebugUtils {
    public static function makeDotGraph(root:BasicBlock):String {
        var blocks = [];
        var edges = [];
        function walk(bb:BasicBlock) {
            blocks.push({id: bb.id, label: haxe.Json.stringify('<${bb.id}>\n' + bb.elements.map(texprToString).join("\n"))});
            for (edge in bb.edges) {
                edges.push({from: bb.id, to: edge.to.id, label: haxe.Json.stringify(edge.label)});
                if (!Lambda.exists(blocks, function(b) return b.id == edge.to.id))
                    walk(edge.to);
            }
        }
        walk(root);

        var blocks = [for (b in blocks) '\t${b.id} [label=${b.label}];'].join("\n");
        var edges = [for (e in edges) '\t${e.from} -> ${e.to} [label=${e.label}];'].join("\n");
        return 'digraph cfg {\n$blocks\n$edges\n}';
    }

    public static function makeVisJsGraph(root:BasicBlock) {
        var blocks = [];
        var edges = [];
        function walk(bb:BasicBlock, level:Int) {
            blocks.push({id: bb.id, label: '<${bb.id}>\n' + bb.elements.map(texprToString).join("\n"), level: level});
            for (edge in bb.edges) {
                edges.push({from: bb.id, to: edge.to.id, label: edge.label, arrows: "to"});
                if (!Lambda.exists(blocks, function(b) return b.id == edge.to.id))
                    walk(edge.to, level + 1);
            }
        }
        walk(root, 0);

        return {
            nodes: blocks,
            edges: edges,
        };
    }

    public static function typeToString(t:Type):String {
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
            case TConst(t):
                var b = new StringBuf();
                b.add("CONST(");
                b.add(typeToString(t));
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

    public static function literalToString(l:TLiteral):String {
        return switch (l) {
            case LInt(i):
                '$i';
            case LBool(b):
                if (b) "true" else "false";
            case LString(s):
                '"${Lexer.escapeString(s)}"';
        };
    }

    public static function texprToString(e:TExpr):String {
        return switch (e.kind) {
            case TVar(v, e):
                var s = 'var ${v.name}';
                if (e != null)
                    s += ' = ' + texprToString(e);
                s;
            case TLocal(v):
                v.name;
            case TThis:
                "this";
            case TAssign(target, e):
                var targetStr = switch (target) {
                    case ATVar(v): v.name;
                    case ATField(eobj, f):
                        var fieldName = switch (f) {
                            case FClassField(_, f): f.name;
                        };
                        texprToString(eobj) + "." + fieldName;
                }
                '$targetStr = ${texprToString(e)}';
            case TLiteral(l):
                literalToString(l);
            case TCall(eobj, args):
                '${texprToString(eobj)}(${args.map(texprToString).join(", ")})';
            case TTuple(values):
                var valuesStr = values.map(texprToString).join(", ");
                if (values.length == 1) valuesStr += ",";
                '($valuesStr)';
            case TTupleElement(etuple, index):
                '(${texprToString(etuple)}).$index';
            case TMethodCall(eobj, f, args):
                var fieldName = switch (f) {
                    case FClassField(_, f): f.name;
                }
                '${texprToString(eobj)}.$fieldName(${args.map(texprToString).join(", ")})';
            case TBinop(op, left, right):
                '${texprToString(left)} ${Printer.printBinop(op)} ${texprToString(right)}';
            case TUnop(op, expr, postfix):
                var s = '(${texprToString(expr)})';
                var op = Printer.printUnop(op);
                if (postfix) s + op else op + s;
            case TVarField(e, f):
                var fieldName = switch (f) {
                    case FClassField(_, f): f.name;
                }
                '${texprToString(e)}.${fieldName}';
            case TMethodClosure(e, f):
                var fieldName = switch (f) {
                    case FClassField(_, f): f.name;
                }
                'METHODCLOSURE<${texprToString(e)}.${fieldName}>';
            case TNew(cl):
                var path = cl.module.concat([cl.name]).join(".");
                'new $path';
            case TReturn(e):
                if (e == null)
                    'return';
                else
                    'return ${texprToString(e)}';
            case TFakeValue:
                throw 'basic block element expressions cannot contain ' + e.kind.getName();
            case TFunction(_, _):
                throw "todo" + e; // ???
        }
    }
}