FMPublic

name := "fm-serializer"

version := "0.4.0-SNAPSHOT"

description := "Scala Macro Based Serialization"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", "2.11.8")

// Needed for the JavaBean tests to work
compileOrder := CompileOrder.JavaThenScala

// NOTE: For -Xelide-below:  ALL == Enabled Assertions,  OFF == Disabled Assertions
scalacOptions := Seq("-unchecked", "-deprecation", "-language:implicitConversions,experimental.macros", "-feature", "-Xlint", "-optimise", "-Yinline-warnings", "-Xelide-below", "OFF")

// We don't want log buffering when running ScalaTest
logBuffered in Test := false

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

// Enable the Macro Paradise Compiler Plugin for Scala 2.10
libraryDependencies <++= (scalaVersion){ sv =>
  if (sv.startsWith("2.10")) Seq (
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    "org.scalamacros" %% "quasiquotes" % "2.1.0"
  ) else Nil
}

// SCALA Libraries
libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "fm-common" % "0.7.0-SNAPSHOT"
)

// JAVA Libraries
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.1",
  "org.joda" % "joda-convert" % "1.8", // Required by joda-time when using Scala
  "org.mongodb" % "bson" % "3.3.0"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
