package puzzles.day2

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day2Part2 extends StrictLogging {
  @tailrec
  def getAnsInner(input: List[String], sum: Long = 0): Long = input match {
    case h :: t =>
      // rawRange should always be an array of 2 items
      val rawRange = h.split('-')
      val range    = rawRange(0).toLong to rawRange(1).toLong
      val total    = range.foldLeft(sum) { (newSum, x) =>
        val xStr             = x.toString
        val xLen             = xStr.length
        val potentialFactors = xLen :: (2 to (xLen / 2)).toList
        val isInvalidId      = potentialFactors.exists(factor => {
          val condition = xLen != 1 && xLen % factor == 0 && xStr.grouped(xLen / factor).toSet.size == 1
          if (condition) logger.debug(s"$xStr --- $factor --- ${xStr.grouped(xLen / factor).toSet}")
          condition
        })
        if (isInvalidId) newSum + x else newSum
      }
      getAnsInner(t, total)
    case Nil => sum
  }

  def getAns(input: List[String]): Long = getAnsInner(input.flatMap(_.split(',').toList))
}
