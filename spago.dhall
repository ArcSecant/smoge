{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "smoge"
, dependencies =
  [ "affjax", "argonaut", "console", "effect", "halogen", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "frontend/src/**/*.purs", "frontend/test/**/*.purs" ]
}
