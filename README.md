# each

Inspired by [the Scala library of the same name](https://github.com/ThoughtWorksInc/each),
each is a Template Haskell library that transforms expressions containing
invocations of impure subexpressions into calls to `fmap`, `<*>`, `join`,
etc. Just mark your impure subexpressions with 'bind' or '~!' and they will be
called appropriately, as in this small demo:

    ghci> $(each [| "Hello, " ++ (~! getLine) |])
    World              <--[keyboard input]
    "Hello, World"

In this case, the code is translated into `fmap ((++) "Hello, ") getLine`

We currently have support for

* Normal function application like `f x y`
* Infix operator application like `x + y`, including sections like `(+ y)`
* Type signatures like `x :: t`

Support for more constructs is coming.

## More demos

A more detailed demo:

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

    ghci> prompt str = putStrLn str *> getLine
    ghci> $(each [| "Nah just " ++ (~! prompt ("What's " ++ bind getLine ++ "?")) |])
    something          <--[keyboard input]
    What's something?
    nothing            <--[keyboard input]
    "Nah just nothing"
