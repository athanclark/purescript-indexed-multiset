{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-aux-multiset"
, dependencies =
  [ "foreign-object"
  , "generics-rep"
  , "ordered-collections"
  , "parseint"
  , "psci-support"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
