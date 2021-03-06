import ReleaseTransformations._

organization := "se.petterarvidsson"

name := "pipesc"

scalaVersion := "2.12.3"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:postfixOps")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)
