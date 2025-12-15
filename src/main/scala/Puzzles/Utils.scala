package puzzles

import java.nio.file.Paths
import scala.io.Source
import scala.util.Using

object Utils {
  private val resourcesPath = Paths.get("src/main/resources")
  private val inputFileName = "input"

  def getInputFromDay(day: String): List[String] =
    Using(Source.fromFile(resourcesPath.resolve(day).resolve(inputFileName).toUri)) { source =>
      source.mkString.split('\n').toList
    }.get
}
