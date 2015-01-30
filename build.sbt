FMPublic

name := "fm-serializer"

version := "0.3.0-SNAPSHOT"

description := "Scala Macro Based Serialization"

scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.10.4", "2.11.5")

// Needed for the JavaBean tests to work
compileOrder := CompileOrder.JavaThenScala

// NOTE: For -Xelide-below:  ALL == Enabled Assertions,  OFF == Disabled Assertions
scalacOptions := Seq("-unchecked", "-deprecation", "-language:implicitConversions,experimental.macros", "-feature", "-Xlint", "-optimise", "-Yinline-warnings", "-Xelide-below", "OFF")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

// Enable the Macro Paradise Compiler Plugin for Scala 2.10
libraryDependencies <++= (scalaVersion){ sv =>
  if (sv.startsWith("2.10")) Seq (
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
    "org.scalamacros" %% "quasiquotes" % "2.0.1"
  ) else Nil
}

// TODO: make these optional
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.6"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"
