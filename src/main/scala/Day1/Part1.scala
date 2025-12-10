package Day1

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val input = Using(Source.fromFile("src/main/scala/Day1/input")) { source => source.mkString.split('\n').toList }.get

  @tailrec
  def getAns(input: List[String], pointer: Int = 50, count: Int = 0): Int = {
    println(s"$pointer --- $count")
    input match {
      case h :: t =>
        val rotations = h.tail.toInt
        val newRawPointer = h.head match {
          case 'L' => pointer - rotations
          case 'R' => pointer + rotations
        }
        val newPointer = math.floorMod(newRawPointer, 100)
        val newCount = if (newPointer == 0) count + 1 else count
        getAns(t, newPointer, newCount)
      case Nil => count
    }
  }

  println(getAns(input))
}
