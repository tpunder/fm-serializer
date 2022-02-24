
//
// Enable Sonatype snapshots repository for SNAPSHOT versions only
//
resolvers ++= {
  if (version.value.trim.endsWith("SNAPSHOT")) Resolver.sonatypeOssRepos("snapshots") else Nil
}

//
// For sbt-sonatype
//
ThisBuild / organization := "com.frugalmechanic"

publishMavenStyle := true

ThisBuild / licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("frugalmechanic", "fm-serializer", "Tim Underwood", "timunderwood@gmail.com"))

//
// For sbt-pgp
//
usePgpKeyHex("AB8A8ACD374B4E2FF823BA35553D700D8BD8EF54")

//
// For sbt-release
//
import ReleaseTransformations._

releaseCrossBuild := true // true if you cross-build the project for multiple Scala versions

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  // For non cross-build projects, use releaseStepCommand("publishSigned")
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)
