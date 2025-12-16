package puzzles.day1

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day1Part2_optimised extends StrictLogging {
  @tailrec
  def getAns(input: List[String], pointer: Int = 50, count: Int = 0, carryOver: Option[Char] = None): Int = {
    logger.debug(s"${input.headOption} --- $pointer --- $count --- $carryOver")
    input match {
      case h :: t =>
        val rotations                       = h.tail.toInt
        val (newRawPointer, carryOverCount) = (h.head, carryOver) match {
          // Edge case: Going left after going right and landing on 0 will double count the 0
          case ('L', Some('R')) => (pointer - rotations, -1)
          case ('L', _)         => (pointer - rotations, 0)
          // Edge case: Going right after going left and landing on 0 will miss the 0
          case ('R', Some('L')) => (pointer + rotations, 1)
          case ('R', _)         => (pointer + rotations, 0)
        }
        val newPointer      = math.floorMod(newRawPointer, 100)
        val additionalCount = math.floorDiv(newRawPointer, 100).abs
        val newCount        = count + additionalCount + carryOverCount
        getAns(t, newPointer, newCount, Option.when(newPointer == 0)(h.head))
      case Nil => count + (if (carryOver.contains('L')) 1 else 0)
    }
  }
}
