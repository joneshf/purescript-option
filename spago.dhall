{ name = "option"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "codec"
  , "codec-argonaut"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functors"
  , "identity"
  , "lists"
  , "maybe"
  , "prelude"
  , "record"
  , "simple-json"
  , "spec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
