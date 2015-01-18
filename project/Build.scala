import sbt._
import Keys._

import com.typesafe.sbt.SbtPgp

object Dependencies {
    val specs2 = "org.specs2" %% "specs2" % "2.3.11" % Test
}

object PDiffBuild extends Build {
    override lazy val settings = super.settings ++ Seq(
        organization := "io.continuum",
        version := "0.1-SNAPSHOT",
        description := "Scala implementation of PerceptualDiff (http://pdiff.sourceforge.net)",
        homepage := Some(url("http://bokeh.github.io/pdiff")),
        licenses := Seq("GPLv2" -> url("http://opensource.org/licenses/GPL-2.0")),
        scalaVersion := "2.11.5",
        crossScalaVersions := Seq("2.10.4", "2.11.5"),
        scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_"),
        scalacOptions in (Compile, doc) := Seq("-groups", "-implicits"),
        shellPrompt := { state =>
            "continuum (%s)> ".format(Project.extract(state).currentProject.id)
        },
        cancelable := true
    )

    lazy val publishSettings = Seq(
        publishTo := {
            val nexus = "https://oss.sonatype.org/"
            if (isSnapshot.value)
                Some("snapshots" at nexus + "content/repositories/snapshots")
            else
                Some("releases" at nexus + "service/local/staging/deploy/maven2")
        },
        publishMavenStyle := true,
        publishArtifact in Test := false,
        pomIncludeRepository := { _ => false },
        pomExtra := (
            <scm>
                <url>https://github.com/bokeh/pdiff</url>
                <connection>scm:git:https://github.com/bokeh/pdiff.git</connection>
            </scm>
            <developers>
                <developer>
                    <id>mattpap</id>
                    <name>Mateusz Paprocki</name>
                    <url>mateuszpaprocki.pl</url>
                </developer>
            </developers>
        ),
        credentials ++= {
            (for {
                username <- sys.env.get("SONATYPE_USERNAME")
                password <- sys.env.get("SONATYPE_PASSWORD")
            } yield {
                Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
            }) orElse {
                val path = Path.userHome / ".sonatype" / "credentials"
                if (path.exists) Some(Credentials(path)) else None
            } toList
        }
    )

    lazy val commonSettings = Defaults.coreDefaultSettings ++ publishSettings ++ Seq(
        parallelExecution in Test := false,
        fork := true
    )

    lazy val pdiffSettings = commonSettings ++ SbtPgp.settings ++ Seq(
        libraryDependencies += Dependencies.specs2,
        initialCommands in Compile := "import io.continuum.pdiff._"
    )

    lazy val pdiff = project in file(".") settings(pdiffSettings: _*)

    override def projects = Seq(pdiff)
}
