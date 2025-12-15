import sbt.*

object Dependencies {
  val common: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    "org.apache.logging.log4j" % "log4j-api" % "2.25.2",
    "org.apache.logging.log4j" % "log4j-core" % "2.25.2",
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.25.2"
  )
}
