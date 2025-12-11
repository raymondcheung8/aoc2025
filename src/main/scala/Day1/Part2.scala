package Day1

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Part2 extends App {
  val input = Using(Source.fromFile("src/main/scala/Day1/input")) { source => source.mkString.split('\n').toList }.get

  @tailrec
  def getAns(input: List[String], pointer: Int = 50, count: Int = 0): Int = {
    if (input.headOption.exists(_.charAt(1) != '0')) println(s"${input.headOption} --- $pointer --- $count")
    input match {
      case h :: t =>
        val rotations = h.tail.toInt
        h.head match {
          case 'L' if rotations == pointer || ((rotations == 0 || rotations == 100) && pointer == 0) => getAns(t, 0, count + 1)
          // Rotations shouldn't ever be 0
          case 'L' if pointer == 0 && rotations != 0 && rotations < 100 => getAns(t, 100 - rotations, count)
          case 'L' if pointer == 0 && rotations == 0                    => getAns(t, 0, count)
          case 'L' if pointer != 0 && rotations < pointer               => getAns(t, pointer - rotations, count)
          case 'L' if pointer == 0 && rotations > 100                   => getAns(s"L0${rotations - 100}" :: t, 0, count + 1)
          case 'L' if pointer != 0 && rotations > pointer               => getAns(s"L0${rotations - pointer}" :: t, 0, count + 1)

          case 'R' if rotations + pointer == 100 => getAns(t, 0, count + 1)
          // Rotations shouldn't ever be 0
          case 'R' if rotations + pointer == 0  => getAns(t, 0, count)
          case 'R' if rotations + pointer > 100 => getAns(s"R0${rotations + pointer - 100}" :: t, 0, count + 1)
          case 'R' if rotations + pointer < 100 => getAns(t, rotations + pointer, count)
        }
      case Nil => count
    }
  }

  println(getAns(input))
}
