package puzzles.day10

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering

object Day10Part2 {
  private def getNewState(state: Map[Int, (Int, Int)], button: Set[Int]): Option[Map[Int, (Int, Int)]] = {
    state
      .foldLeft(Option(Map.empty[Int, (Int, Int)])) {
        case (Some(acc), (i, (joltage, joltageNeeded))) if button(i) && joltageNeeded > 0   => Some(acc + (i -> (joltage + 1, joltageNeeded - 1)))
        case (Some(acc), (i, (joltage, joltageNeeded))) if !button(i) && joltageNeeded >= 0 => Some(acc + (i -> (joltage, joltageNeeded)))
        case _                                                                              => None
      }
      .map(newState => newState)
  }

  @tailrec
  private def getMinButtonCount(
      targetState: Map[Int, (Int, Int)],
      buttons: Vector[Set[Int]],
      queue: Vector[(Map[Int, (Int, Int)], Int, Int)],
      foundStates: Set[Map[Int, (Int, Int)]] = Set()
  ): Option[Int] = {
    val (state, totalJoltageNeeded, count) = queue.head
    val newStates                          = buttons.flatMap(getNewState(state, _)).toSet
    lazy val diffStates                    = newStates diff foundStates
    lazy val allFoundStates                = foundStates union newStates
//    println(s"${state.map(_._2._2).max} --- $state")

    if (newStates(targetState)) {
//      println(s"0 --- ${targetState}")
      Some(count + 1)
    } else {
      val threshold            = 14
      val greedyQueue          = (queue.tail ++ diffStates.map(newState => (newState, newState.map(_._2._2).sum, count + 1))).sortBy(_._2)
      val greedyHeadRemainders = greedyQueue.tail.head._1.map(_._2._2)
      // We want to sort the list by the lexicographic minimum of the light joltages in descending order
      val sortedQueue =
        if (greedyHeadRemainders.max - greedyHeadRemainders.min < threshold) greedyQueue
        else (queue.tail ++ diffStates.map(state => (state, state.map(_._2._2).sum, count + 1))).sortBy(_._1.map(_._2._2).toList.sortBy(-_))
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
      val startingState = Vector((joltages.view.mapValues((0, _)).toMap, joltages.values.sum, 0))
      val targetState   = joltages.view.mapValues((_, 0)).toMap
      getMinButtonCount(targetState, buttons, startingState).get
    }.sum
  }
}
