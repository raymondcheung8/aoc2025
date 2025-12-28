package puzzles.day10

import scala.annotation.tailrec

object Day10Part1 {
  private def xor(state: Set[Int], button: Set[Int]): Set[Int] = (state union button) diff (state intersect button)

  @tailrec
  private def getMinButtonCount(
      targetState: Set[Int],
      buttons: Vector[Set[Int]],
      foundStates: Set[Set[Int]] = Set(),
      queue: Vector[(Set[Int], Int)] = Vector((Set(), 0))
  ): Option[Int] = {
    val (state, count)      = queue.head
    val newStates           = buttons.map(xor(state, _)).toSet
    lazy val diffStates     = newStates diff foundStates
    lazy val allFoundStates = foundStates union newStates

    if (newStates(targetState)) Some(count + 1)
    else getMinButtonCount(targetState, buttons, allFoundStates, queue.tail ++ diffStates.map((_, count + 1)))
  }

  def getAns(input: List[String]): Int = {
    val machines = input.map(_.split(' ').map(_.toVector).toVector match {
      case ('[' +: h :+ ']') +: t :+ ('{' +: l :+ '}') =>
        (
          h.zipWithIndex.collect { case ('#', i) => i }.toSet,
          t.map { case '(' +: wiring :+ ')' => wiring.mkString.split(',').map(_.toInt).toSet },
          l.mkString.split(',').map(_.toInt)
        )
    })
    machines.map { case (lights, buttons, joltage) =>
      getMinButtonCount(lights, buttons).get
    }.sum
  }
}
