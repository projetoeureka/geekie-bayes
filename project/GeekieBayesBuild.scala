import sbt._
import sbt.Keys._

object GeekieBayesBuild extends Build {

  val dependencies = {
    Seq(
      "io.spray" %% "spray-json" % "1.3.1",
      "com.typesafe.akka" %% "akka-actor" % "2.3.9",
      "org.json4s" %% "json4s-jackson" % "3.2.11",
      "org.apache.commons" % "commons-math3" % "3.5",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.3" % "test",
      //linear algebra libraries
      "org.scalanlp" %% "breeze" % "0.10",
      "org.scalanlp" %% "breeze-natives" % "0.10",
      "org.scalanlp" %% "breeze-viz" % "0.10"
    )
  }

  lazy val GeekieBayesProject = Project("geekie-bayes", file(".")) settings(
    version := "1.0",
    scalaVersion := "2.11.6",
    libraryDependencies ++= dependencies,
    resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/")
    )
}
