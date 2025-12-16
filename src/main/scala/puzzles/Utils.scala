package puzzles

import java.nio.file.{Path, Paths}
import scala.io.Source
import scala.util.Using

object Utils {
  private val resourcesPath   = Paths.get("src/main/resources")
  private val inputFileName   = "input"
  private val answersFileName = "answers"

  private def readFile(path: Path): List[String] = Using(Source.fromFile(path.toUri)) { _.mkString.split('\n').toList }.get

  def getInputFromDay(day: String): List[String] = readFile(resourcesPath.resolve(day).resolve(inputFileName))

  def getAnswers: Map[(String, String), Long] =
    readFile(resourcesPath.resolve(answersFileName))
      .map(_.split(',') match { case Array(day, part, answer, _*) => (day, part) -> answer.trim.toLong })
      .toMap
}
