package puzzles.day2

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day2Part2v4 extends StrictLogging {
  @tailrec
  def getAnsInner(input: List[String], sum: Long = 0): Long = input match {
    case h :: t =>
      // rawRange should always be an array of 2 items
      val rawRange = h.split('-')
      val range    = rawRange(0).toLong to rawRange(1).toLong
      val total    = range.foldLeft(sum) { (newSum, x) =>
        val xStr        = x.toString
        val xLen        = xStr.length
        val subStrLens  = (1 to (xLen / 2)).toList
        val isInvalidId = subStrLens.exists(subStrLen => {
          val factor    = xLen / subStrLen
          val condition = xLen != 1 && xLen % subStrLen == 0 && xStr.substring(0, subStrLen) * factor == xStr
          if (condition) logger.debug(s"$xStr --- $subStrLen --- ${xStr.substring(0, subStrLen) * factor}")
          condition
        })
        if (isInvalidId) newSum + x else newSum
      }
      getAnsInner(t, total)
    case Nil => sum
  }

  def getAns(input: List[String]): Long = getAnsInner(input.flatMap(_.split(',').toList))
}
