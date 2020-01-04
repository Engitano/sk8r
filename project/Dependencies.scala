import sbt._

object Dependencies {

  val scalaTest    = "org.scalatest" %% "scalatest"     % "3.0.8"
  val http4sClient = "org.http4s"    %% "http4s-client" % "0.20.15"
  val circeGeneric = "io.circe"      %% "circe-generic" % "0.12.3"
  val circeParser  = "io.circe"      %% "circe-parser"  % "0.12.3"
  val http4sCirce  = "org.http4s"    %% "http4s-circe"  % "0.20.15"
}
