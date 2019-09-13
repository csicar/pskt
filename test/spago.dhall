{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "prelude", "effect", "console", "math", "integers", "refs", "ordered-collections" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
