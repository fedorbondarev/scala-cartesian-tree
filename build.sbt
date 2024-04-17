ThisBuild / scalaVersion := "2.13.11"
ThisBuild / version := "0.1.0-SNAPSHOT"

Compile / compile / scalacOptions ++= Seq(
  "-Werror",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused",
  "-Wvalue-discard",
  "-Xlint",
  "-Xlint:-byname-implicit",
  "-Xlint:-implicit-recursion",
  "-unchecked"
)

lazy val root = (project in file("."))
  .settings(
    name := "scala-cartesian-tree",
    libraryDependencies ++= Seq(
      // Cats
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-effect" % "3.5.0",

      // Tests
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    ),
    scalacOptions += "-Ymacro-annotations",
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  )
