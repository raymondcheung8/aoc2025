package puzzles.day2

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day2Part1 extends StrictLogging {
  @tailrec
  def getAnsInner(input: List[String], sum: Long = 0): Long = input match {
    case h :: t =>
      // rawRange should always be an array of 2 items
      val rawRange = h.split('-')
      logger.debug(s"$h --- ${rawRange(0).toLong} --- ${rawRange(1).toLong}")
      val range = rawRange(0).toLong to rawRange(1).toLong
      val total = range.foldLeft(sum) { (newSum, x) =>
        val xStr = x.toString
        val xLen = xStr.length
        if (xLen % 2 == 0 && xStr.grouped(xLen / 2).toSet.size == 1) newSum + x else newSum
      }
      getAnsInner(t, total)
    case Nil => sum
  }

  def getAns(input: List[String]): Long = getAnsInner(input.flatMap(_.split(',').toList))
}
