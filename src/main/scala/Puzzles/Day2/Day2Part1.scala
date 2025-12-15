package puzzles.day2

import puzzles.Utils

import scala.annotation.tailrec

object Day2Part1 extends App {
  val input = Utils.getInputFromPath("src/main/scala/puzzles/day2/input")

  @tailrec
  def getAns(input: List[String], sum: Long = 0): Long = input match {
    case h :: t =>
      // rawRange should always be an array of 2 items
      val rawRange = h.split('-')
      println(s"$h --- ${rawRange(0).toLong} --- ${rawRange(1).toLong}")
      val range = rawRange(0).toLong to rawRange(1).toLong
      val total = range.foldLeft(sum) { (newSum, x) =>
        val xStr = x.toString
        val xLen = xStr.length
        if (xLen % 2 == 0 && xStr.grouped(xLen / 2).toSet.size == 1) newSum + x else newSum
      }
      getAns(t, total)
    case Nil => sum
  }

  println(getAns(input.head.split(',').toList))
}
