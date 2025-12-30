package puzzles.day10

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering

object Day10Part2 {
  private case class State(state: Map[Int, (Int, Int)])

  private def getNewState(state: Map[Int, (Int, Int)], button: Set[Int]): Option[State] = {
    state
      .foldLeft(Option(Map.empty[Int, (Int, Int)])) {
        case (Some(acc), (i, (joltage, joltageNeeded))) if button(i) && joltageNeeded > 0   => Some(acc + (i -> (joltage + 1, joltageNeeded - 1)))
        case (Some(acc), (i, (joltage, joltageNeeded))) if !button(i) && joltageNeeded >= 0 => Some(acc + (i -> (joltage, joltageNeeded)))
        case _                                                                              => None
      }
      .map(newState => State(newState))
  }

  @tailrec
  private def getMinButtonCount(
      targetState: State,
      buttons: Vector[Set[Int]],
      queue: Vector[(State, Int)],
      foundStates: Set[State] = Set()
  ): Option[Int] = {
    val (State(state), count) = queue.head
    val newStates             = buttons.flatMap(getNewState(state, _)).toSet
    lazy val diffStates       = newStates diff foundStates
    lazy val allFoundStates   = foundStates union newStates
//    println(s"$totalJoltage(${targetState.totalJoltage}) --- $state")
    println(s"${state.map(_._2._2).max} --- $state")

    if (newStates(targetState)) {
      println(s"0 --- ${targetState.state}")
      Some(count + 1)
    } else {
      // We want to sort the list by the lexicographic minimum of the light joltages in descending order
      val sortedQueue = (queue.tail ++ diffStates.map((_, count + 1))).sortBy(_._1.state.map(_._2._2).toList.sortBy(-_))
      getMinButtonCount(targetState, buttons, sortedQueue, allFoundStates)
    }
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
      println(s"starting machine...")
      val startingState = Vector((State(joltages.view.mapValues((0, _)).toMap), 0))
      val targetState   = State(joltages.view.mapValues((_, 0)).toMap)
      getMinButtonCount(targetState, buttons, startingState).get
    }.sum
  }
}
