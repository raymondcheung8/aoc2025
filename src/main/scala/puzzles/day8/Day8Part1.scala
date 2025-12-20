package puzzles.day8

import com.typesafe.scalalogging.StrictLogging

object Day8Part1 extends StrictLogging {
  private def square(x: Double): Double = math.pow(x, 2)

  private def euclidean(connection: List[(Long, Long, Long)]): Double = connection match {
    case List((x, y, z), (x2, y2, z2), _*) => math.sqrt(square(x - x2) + square(y - y2) + square(z - z2))
  }

  def getAns(input: List[String], noOfConnections: Int): Int = {
    val positions = input.map(_.split(',') match { case Array(x, y, z, _*) => (x.toLong, y.toLong, z.toLong) })

    val shortestDistances = positions
      .combinations(2)
      .map(connection => (connection, euclidean(connection)))
      .toList
      .sortBy(_._2)

    val circuits = shortestDistances
      .foldLeft(Set[Set[(Long, Long, Long)]](), noOfConnections) {
        case ((circuits, remainingConnections), (connection, _)) if remainingConnections != 0 =>
          connection.flatMap(pos => circuits.filter(_(pos))) match {
            case matchingCircuits if matchingCircuits.length == 2 =>
              val circuitsSet = matchingCircuits.toSet
              ((circuits diff circuitsSet) + circuitsSet.flatten, remainingConnections - 1)
            case matchingCircuits if matchingCircuits.length == 1 =>
              (circuits.map(circuit => if (connection.exists(circuit(_))) circuit ++ connection else circuit), remainingConnections - 1)
            case _ => (circuits + connection.toSet, remainingConnections - 1)
          }
        case (acc, _) => acc
      }
      ._1

    logger.debug(s"${shortestDistances.mkString("\n")}")
    logger.debug(s"${circuits.mkString("\n")}")
    logger.debug(s"${circuits.map(_.size).toList.sortBy(-_).take(3)}")
    circuits.map(_.size).toList.sortBy(-_).take(3).product
  }
}
