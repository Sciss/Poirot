name         := "Poirot"

version      := "0.1.0"

organization := "de.sciss"

scalaVersion := "2.10.3"

description  := "A Scala front-end for the JaCoP constraints solver library"

homepage     := Some(url("https://github.com/Sciss/" + name.value))

licenses     := Seq("AGPL v3" -> url("http://www.gnu.org/licenses/agpl-3.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" % "jacop" % "3.3.+"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- console ----

initialCommands in console :=
  """import de.sciss.poirot._
    |import Implicits._
    |""".stripMargin

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

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

