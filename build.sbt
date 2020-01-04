import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / organization     := "com.engitano"
ThisBuild / organizationName := "Engitano"

val majorVersion = SettingKey[String]("major version")
val minorVersion = SettingKey[String]("minor version")
val patchVersion = SettingKey[Option[String]]("patch version")

Global / majorVersion := "0"
Global / minorVersion := "1"
Global / patchVersion := Some("0")

val writeVersion = taskKey[Unit]("Writes the version to version.txt")
writeVersion := {
  IO.write(baseDirectory.value / "version.txt", (`fs2-firestore`  / version).value)
}

lazy val root = (project in file("."))
  .settings(
    name := "sk8r",
    version := s"${majorVersion.value}.${minorVersion.value}${patchVersion.value.fold("")(p => s".$p")}",
    startYear := Some(2019),
    licenses += ("MIT", new URL("http://opensource.org/licenses/MIT")),
    libraryDependencies ++= Seq(
      http4sClient,
      http4sCirce,
      circeGeneric,
      circeParser,
      scalaTest % Test
    ),
    bintrayOrganization := Some("engitano"),
    bintrayPackageLabels := Seq("kubernetes", "scala"),
    sourceGenerators in Compile += Def.task {
      CodeGen.generate((sourceManaged in Compile).value, (resourceDirectory in Compile).value / "eks.k8s.api.json")
    }.taskValue
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
