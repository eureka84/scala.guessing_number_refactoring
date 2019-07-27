name := "guess_number_refactoring"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"