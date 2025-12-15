import sbt.*

object Dependencies {
  val common: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
}
