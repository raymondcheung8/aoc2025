package puzzles.day3

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day3Part2 extends StrictLogging {
  @tailrec
  private def getMaxJoltage(bank: List[Char], maxPrevBatt: Int = 0, maxJoltage: Int = 0): Int = bank match {
    case h :: t => getMaxJoltage(t, maxPrevBatt.max(h.asDigit), maxJoltage.max(s"$maxPrevBatt$h".toInt))
    case Nil => maxJoltage
  }

  @tailrec
  def getAns(input: List[String], acc: Int = 0): Int = input match {
    case h :: t => getAns(t, acc + getMaxJoltage(h.toList))
    case Nil => acc
  }
}
