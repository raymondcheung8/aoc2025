package puzzles.day1

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day1Part1 extends StrictLogging {
  @tailrec
  def getAns(input: List[String], pointer: Int = 50, count: Int = 0): Int = {
    logger.debug(s"$pointer --- $count")
    input match {
      case h :: t =>
        val rotations     = h.tail.toInt
        val newRawPointer = h.head match {
          case 'L' => pointer - rotations
          case 'R' => pointer + rotations
        }
        val newPointer = math.floorMod(newRawPointer, 100)
        val newCount   = if (newPointer == 0) count + 1 else count
        getAns(t, newPointer, newCount)
      case Nil => count
    }
  }
}
