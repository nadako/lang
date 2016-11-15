Aside from normal features already found in Haxe, it would be cool to implement this
in my toy language (in no particular order):

 * Immutable types. For this, i'm thinking we could have a `TImmutable(t:Type)` type
   constructor and prevent mutating operations on it. field var access on an `TImmutable`
   should yield a `TImmutable` type as well, as for function calls, an explicit specifier
   could be used for starters, but later it could infer whether it mutates the instance or not.

 * Null-safety. Assume all types non-nullable, unless they are annotated somehow, forbid using
   `null` for non-nullable types, provide sugar for easy unwrapping. Ideally, also provide
   automatic unwrapping on `if (some != null)` checks, but that's gonna require control flow
   analysis.

 * Type classes AKA Rust-style traits. This is a really powerful concept with which we
   can provide required behaviour to any existing type without run-time overhead (via
   static dispatch).

 * "Proper" OOP. Not a fan of it personally, but it's required for languages interop
   and some people still do a lot of OOP. By "proper" i mean: abstract classes, protected fields,
   also everything final/sealed (non-inheritable, non-virtual) by default, requiring a keyword
   to enable subclassing (similar to Kotlin's "open").

 * Tuples. Yes, please! Also, with matching and inlining support.

 * Flexible variance settings or even inference, so they are not always invariant like in Haxe.
   This is gonna be an adventure, but an exciting one.

 * Fail after typing functions if argument or return types are still unknown. Better safe than sorry.

 * Simplier module system where there's no packages, but only modules and declarations in them.
   Hierarchy could be represented by submodules which are modules in a directory (in which case,
   the root module declarations would live in a file with a special name, like `module.hx`).

 * Top-level module functions. Handy for small programs and utilities.

 * Matching + destructuring on `var` (or maybe better call it `let`) bindings, similar to
   Rust. Why limit ourselves to `switch` when we can do it everywhere. Especially in `if`
   it's very handy.

 * Short lambda. Simple as that.

 * No Dynamic, no untyped, so a static analyzer can reason about the AST. Have to think about
   some ways to get some reflection here though.

 * Function and operator overloading. This is a bit controversial and tricky to implement,
   but I should try nevertheless. Without Dynamic nonsense we could fairly easy generate multiple
   functions with different postfix names and call to them on targets that don't support native
   overloading.

 * AST-level conditional compilation directives instead of token-level, only in places where it makes sense.
   This way it's gonna be actually possible to write a decent code formatter.

 * Roslyn-like AST structures. This is something I'm not sure about, but the appealing
   fact is that their AST always contain full info to recreate a full source file after
   modification. This could ease the pain for writing refactoring tools.
