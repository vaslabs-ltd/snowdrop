import sbt._

object Dependencies {

  object Library {
    object circe {
      val core = "io.circe" %% "circe-core" % Version.circe
    }
    val scalatest =
      "org.scalatest" %% "scalatest" % Version.scalatest % "test"
  }

  object Version {
    val circe = "0.14.2"
    val scalatest = "3.2.12"
  }

  object Modules {
    val `snowdrop-circe` =
      Seq(
        Library.circe.core,
        Library.scalatest
      )
  }
}
