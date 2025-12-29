package puzzles.day10

import scala.annotation.tailrec

object Day10Part2 {
  private def getNewState(state: Map[Int, Int], button: Set[Int], targetState: Map[Int, Int]): Option[Map[Int, Int]] = {
    state.foldLeft(Option(Map.empty[Int, Int])) {
      case (Some(acc), (i, joltage)) if button(i) && joltage < targetState(i)   => Some(acc + (i -> (joltage + 1)))
      case (Some(acc), (i, joltage)) if !button(i) && joltage <= targetState(i) => Some(acc + (i -> joltage))
      case _                                                                    => None
    }
  }

  @tailrec
  private def getMinButtonCount(
      targetState: Map[Int, Int],
      buttons: Vector[Set[Int]],
      queue: Vector[(Map[Int, Int], Int)],
      foundStates: Set[Map[Int, Int]] = Set()
  ): Option[Int] = {
    val (state, count)      = queue.head
    val newStates           = buttons.flatMap(getNewState(state, _, targetState)).toSet
    lazy val diffStates     = newStates diff foundStates
    lazy val allFoundStates = foundStates union newStates

    if (newStates(targetState)) Some(count + 1)
    else getMinButtonCount(targetState, buttons, queue.tail ++ diffStates.map((_, count + 1)), allFoundStates)
  }

  def getAns(input: List[String]): Int = {
    val machines = input.map(_.split(' ').map(_.toVector).toVector match {
      case ('[' +: h :+ ']') +: t :+ ('{' +: l :+ '}') =>
        (
          h.zipWithIndex.collect { case ('#', i) => i }.toSet,
          t.map { case '(' +: wiring :+ ')' => wiring.mkString.split(',').map(_.toInt).toSet },
          l.mkString.split(',').zipWithIndex.map { case (joltage, i) => i -> joltage.toInt }.toMap
        )
    })
    machines.map { case (lights, buttons, joltages) =>
      val startingState = Vector((joltages.view.mapValues(_ => 0).toMap, 0))
      getMinButtonCount(joltages, buttons, startingState).get
    }.sum
  }
}
