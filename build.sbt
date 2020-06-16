lazy val commonSettings = Seq(
  name         := "Poirot",
  version      := "0.3.1-SNAPSHOT",
  organization := "de.sciss",
  scalaVersion := "2.13.2",
  crossScalaVersions := Seq("2.12.11", "2.13.2"),
  description  := "A Scala front-end for the JaCoP constraints solver library",
  homepage     := Some(url("https://github.com/Sciss/" + name.value)),
  licenses     := Seq("AGPL v3" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
    libraryDependencies ++= Seq(
    "de.sciss" % "jacop" % "3.5.0"
  ),
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xsource:2.13"),
  updateOptions := updateOptions.value.withLatestSnapshots(false),
  // ---- console ----
  initialCommands in console :=
    """import de.sciss.poirot._
      |import Implicits._
      |""".stripMargin
)

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = name.value
  <scm>
    <url>git@github.com:Sciss/{n}.git</url>
    <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
  </scm>
    <developers>
      <developer>
        <id>sciss</id>
        <name>Hanns Holger Rutz</name>
        <url>http://www.sciss.de</url>
      </developer>
      <developer>
        <id>kris</id>
        <name>Krzysztof Kuchcinski</name>
        <email>krzysztof.kuchcinski@cs.lth.se</email>
        <roles>
          <role>Core developer</role>
        </roles>
        <organization>cs.lth.se</organization>
        <timezone>+1</timezone>
      </developer>
    </developers>
  }
)

