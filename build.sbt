import de.johoop.jacoco4sbt._
import JacocoPlugin._
 
name := "oj"

organization := "cf.oj"

version := "1.0"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

//Define dependencies. These ones are only required for Test and Integration Test scopes.
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0.M8" % "test, it",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test,it"
)

// For Settings/Task reference, see http://www.scala-sbt.org/release/sxr/sbt/Keys.scala.html

// Compiler settings. Use scalac -X for other options and their description.
// See Here for more info http://www.scala-lang.org/files/archive/nightly/docs/manual/html/scalac.html 
scalacOptions ++= List("-feature","-deprecation", "-unchecked")

// ScalaTest settings.
// Ignore tests tagged as @Slow (they should be picked only by integration test)
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "org.scalatest.tags.Slow")

//Code Coverage section
jacoco.settings

//itJacoco.settings

//Style Check section 
org.scalastyle.sbt.ScalastylePlugin.Settings
 
org.scalastyle.sbt.PluginKeys.config <<= baseDirectory { _ / "src/main/config" / "scalastyle-config.xml" }

// Generate Eclipse project with sources for dependencies
EclipseKeys.withSource := true
