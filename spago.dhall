{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-indexed-multiset"
, dependencies =
  [ "argonaut"
  , "arraybuffer-class"
  , "foreign-object"
  , "generics-rep"
  , "ordered-collections"
  , "parseint"
  , "psci-support"
  , "quickcheck"
  , "intmap"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
