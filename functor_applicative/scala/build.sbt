name := "Applicative Functor WS"

scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")