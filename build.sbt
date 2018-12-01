name := "Probability"

organization := "com.github.jonnylaw"

version := "0.1"

scalaVersion := "2.12.7"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  Resolver.sonatypeRepo("public")
)

// this is for simulacrum typeclass support
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-viz" % "0.13.2",
  "com.github.fommil.netlib" % "all" % "1.1.2",
  "com.nrinaudo" %% "kantan.csv-cats" % "0.4.0",
  "com.stripe" %% "rainier-core" % "0.2.0",
  "org.typelevel" %% "cats-core" % "1.4.0",
  "org.typelevel" %% "cats-free" % "0.9.0",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)
