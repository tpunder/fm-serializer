name := "fm-serializer"

description := "Scala Macro Based Serialization"

scalaVersion := "3.3.0"

crossScalaVersions := Seq("3.3.0", "2.13.10", "2.12.17", "2.11.12")

// Needed for the JavaBean tests to work
compileOrder := CompileOrder.JavaThenScala

// NOTE: For -Xelide-below:  ALL == Enabled Assertions,  OFF == Disabled Assertions
scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-language:implicitConversions,experimental.macros",
  "-feature",
  "-Xlint",
  "-Xelide-below", "OFF",
) ++ (if (scalaVersion.value.startsWith("2.12") || scalaVersion.value.startsWith("2.13")) Seq(
  // Scala 2.12 specific compiler flags
  "-opt:l:inline",
  "-opt-inline-from:<sources>",
) else Nil) ++ (if (scalaVersion.value.startsWith("3.")) Seq(
  // Scala 3 specific compiler flags
  "-Xcheck-macros",
) else Nil)

// Due to Scala Standard Library changes in 2.13 some code is specific to
// Scala 2.12 and below (e.g. 2.11, 2.12) and some code is specific to 2.13
// and higher (e.g. 2.13, 3.0).
Compile / unmanagedSourceDirectories += {   
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n < 13 => sourceDirectory.value / "main" / "scala-2.12-"
    case _ => sourceDirectory.value / "main" / "scala-2.13+"
  }
}

// Need to make sure any Java sources are compiled to 1.8 classfile format
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// We don't want log buffering when running ScalaTest
Test / logBuffered := false

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
    case _ => Nil
  }
}

// SCALA Libraries
libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "fm-common" % "1.0.1",
  "com.frugalmechanic" %% "fm-json" % "1.0.0"
)

// JAVA Libraries
libraryDependencies ++= Seq(
  "com.sun.xml.bind" % "jaxb-core" % "2.3.0.1", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "com.sun.xml.bind" % "jaxb-impl" % "2.3.1", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "javax.xml.bind" % "jaxb-api" % "2.3.1", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "javax.activation" % "javax.activation-api" % "1.2.0", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "joda-time" % "joda-time" % "2.9.1",
  "org.joda" % "joda-convert" % "1.8", // Required by joda-time when using Scala
  "org.mongodb" % "bson" % "3.3.0"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

publishTo := sonatypePublishToBundle.value

//ThisBuild / versionScheme := Some("early-semver")
