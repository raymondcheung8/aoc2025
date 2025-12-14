package Day2

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day2Part1 extends App {
  val input = Using(Source.fromFile("src/main/scala/Day2/input")) { source => source.mkString.split('\n').toList }.get

  @tailrec
  def getAns(input: List[String], sum: Long = 0): Long = input match {
    case h :: t =>
      // rawRange should always be an array of 2 items
      val rawRange = h.split('-')
      println(h)
      println(rawRange(0).toLong)
      println(rawRange(1).toLong)
      val range = rawRange(0).toLong to rawRange(1).toLong
      val total = range.foldLeft(sum) { (newSum, x) =>
        val xStr = x.toString
        val xLen = xStr.length
        if (xLen % 2 == 0 && xStr.grouped(xLen / 2).toSet.size == 1) newSum + x else newSum
      }
      getAns(t, total)
    case Nil    => sum
  }

  println(getAns(input.head.split(',').toList))
}
