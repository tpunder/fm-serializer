name := "fm-serializer"

organization := "com.frugalmechanic"

version := "0.1.0-SNAPSHOT"

description := "LazySeq"

licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/frugalmechanic/fm-serializer"))

scalaVersion := "2.10.4"

// Note: Use "++ 2.11.0" to select a specific version when building
crossScalaVersions := Seq("2.10.4", "2.11.0")

// Needed for the JavaBean tests to work
compileOrder := CompileOrder.JavaThenScala

// NOTE: For -Xelide-below:  ALL == Enabled Assertions,  OFF == Disabled Assertions
scalacOptions := Seq("-unchecked", "-deprecation", "-language:implicitConversions,experimental.macros", "-feature", "-optimise", "-Yinline-warnings", "-Xelide-below", "OFF")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

// Enable the Macro Paradise Compiler Plugin for Scala 2.10
libraryDependencies <++= (scalaVersion){ sv =>
  if (sv.startsWith("2.10")) Seq (
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full),
    "org.scalamacros" %% "quasiquotes" % "2.0.0"
  ) else Nil
}

// TODO: make these optional
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.6"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>tim</id>
      <name>Tim Underwood</name>
      <email>tim@frugalmechanic.com</email>
      <organization>Frugal Mechanic</organization>
      <organizationUrl>http://frugalmechanic.com</organizationUrl>
    </developer>
  </developers>
  <scm>
      <connection>scm:git:git@github.com:frugalmechanic/fm-serializer.git</connection>
      <developerConnection>scm:git:git@github.com:frugalmechanic/fm-serializer.git</developerConnection>
      <url>git@github.com:frugalmechanic/fm-serializer.git</url>
  </scm>)

