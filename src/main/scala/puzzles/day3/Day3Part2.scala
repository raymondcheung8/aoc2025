package puzzles.day3

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day3Part2 extends StrictLogging {
  @tailrec
  private def getBatts(batts: Vector[Char], pointer: Int = 0): Vector[Char] = {
    if (pointer + 1 < batts.length)
      if (batts(pointer) < batts(pointer + 1)) batts.patch(pointer, Nil, 1)
      else getBatts(batts, pointer + 1)
    else batts.take(pointer)
  }

  @tailrec
  private def getMaxJoltage(bank: List[Char], currentBatts: Vector[Char]): Long = bank match {
    case h :: t => getMaxJoltage(t, getBatts(currentBatts :+ h))
    case Nil =>
      logger.debug(s"${currentBatts.mkString}")
      currentBatts.mkString.toLong
  }

  @tailrec
  def getAns(input: List[String], acc: Long = 0L): Long = input match {
    case h :: t =>
      val (initial, remainingBank) = h.splitAt(12)
      getAns(t, acc + getMaxJoltage(remainingBank.toList, initial.toVector))
    case Nil => acc
  }
}
