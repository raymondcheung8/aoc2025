package Puzzles

import scala.io.Source
import scala.util.Using

object Utils {
  def getInputFromPath(path: String): List[String] = Using(Source.fromFile(path)) { source => source.mkString.split('\n').toList }.get
}
