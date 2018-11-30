# each

Inspired by [the Scala library of the same name](https://github.com/ThoughtWorksInc/each),
each is a Template Haskell library that transforms expressions containing
invocations of impure subexpressions into do-notation. Just mark your impure
subexpressions with `bind` or `~!` and they will be called appropriately,
as in this small demo:

    ghci> :m Each
    ghci> $(each [| "Hello, " ++ (~! getLine) |])
    World              <--[keyboard input]
    "Hello, World"

With the `ApplicativeDo` GHC extension, calls to `fmap` and `<*>` will be
arranged so that you don't need to worry if you use, say, Haxl and needs
`Applicative` for parallelism.

Most constructs where this would make things much more simpler are already
supported. In particular, these are okay:

- Nested `bind`s.
- Branching constructs, even if the branches themselves uses `bind`. The
generated `do`-notation will generally match imperative intuition.

These are some quirks:

- `let` expressions are evaluated sequentially. `each` currently lacks support
for detecting pure `let` expressions.
- `where` is not implemented.
- Parameters to lambda functions may not be used impurely. This is acceptable,
but the error message may be confusing:

        ghci> :m Each
        ghci> $(each [| (\x -> bind x) |])

        <interactive>:25:3: error:
        • The exact Name ‘x_acBv’ is not in scope
        Probable cause: you used a unique Template Haskell name (NameU),
        perhaps via newName, but did not bind it
        If that's it, then -ddump-splices might be useful
        • In the untyped splice: $(each [| (\ x -> bind x) |])

  Also, `bind`s in the lambda will be run when the lambda is *constructed*,
not when it's called.
- `PatternGuard`, `LambdaCase` and a few other extensions (uncertain) are not
yet implemented.

If you find something wrong, or really want some feature, feel free to leave an
issue.

## How it works

The basic structure of an `each` block is this:

```haskell
$(each [| ... |])
```

Inside of this block, three (interchangable) ways are used to mark impure
subexpressions:

- `bind expr`
- `bind $ expr`
- `(~! expr)`

`do`-notation is generated according to left-to-right order, and branching is
handled.

## More demos

A more detailed demo:

    ghci> :m Each
    ghci> :{
        | $(each [|
        |   "Hey it works"
        |   ++ show (length $
        |     "something"
        |     ++ (~! readFile "/etc/issue")
        |     ++ (~! readFile "/etc/issue.net"))
        | |])
        | :}
    "Hey it works64"

Nested binds also work as expected.

    ghci> :m Each
    ghci> prompt str = putStrLn str *> getLine
    ghci> $(each [| "Nah just " ++ (~! prompt ("What's " ++ bind getLine ++ "?")) |])
    something          <--[keyboard input]
    What's something?
    nothing            <--[keyboard input]
    "Nah just nothing"
