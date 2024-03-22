{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "ace"
  , "aff"
  , "aff-promise"
  , "argonaut"
  , "arrays"
  , "console"
  , "control"
  , "dom-indexed"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "halogen"
  , "halogen-formless"
  , "halogen-subscriptions"
  , "integers"
  , "js-bigints"
  , "maybe"
  , "newtype"
  , "ocelot"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "simple-json"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-workers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
