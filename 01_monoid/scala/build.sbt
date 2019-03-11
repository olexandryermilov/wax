name := "Monoid WS"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC"
libraryDependencies += "com.twitter" %% "algebird-core" % "0.13.4"

