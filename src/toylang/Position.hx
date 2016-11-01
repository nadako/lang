package toylang;

class Position {
    public var file:String;
    public var min:Int;
    public var max:Int;

    public function new(file, min, max) {
        this.file = file;
        this.min = min;
        this.max = max;
    }

    public function toString():String {
        return '$file:$min-$max';
    }

    public static var nullPos(default,never) = new Position("", 0, 0);

    public static function union(a:Position, b:Position):Position {
        var min = if (a.min < b.min) a.min else b.min;
        var max = if (a.max > b.max) a.max else b.max;
        return new Position(a.file, min, max);
    }
}
