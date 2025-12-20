package puzzles.day8

import com.typesafe.scalalogging.StrictLogging

object Day8Part2 extends StrictLogging {
  private def square(x: Double): Double = math.pow(x, 2)

  private def euclidean(connection: List[(Long, Long, Long)]): Double = connection match {
    case List((x, y, z), (x2, y2, z2), _*) => math.sqrt(square(x - x2) + square(y - y2) + square(z - z2))
  }

  def getAns(input: List[String]): Long = {
    val positions     = input.map(_.split(',') match { case Array(x, y, z, _*) => (x.toLong, y.toLong, z.toLong) })
    val noOfPositions = positions.length

    val shortestDistances = positions
      .combinations(2)
      .map(connection => (connection, euclidean(connection)))
      .toList
      .sortBy(_._2)

    shortestDistances
      .foldLeft(Set[Set[(Long, Long, Long)]](), 0, Option.empty[Long]) {
        case ((circuits, noOfConnections, ans), (connection, _)) if noOfConnections < noOfPositions - 1 =>
          val (newCircuits, newNoOfConnections) = connection.flatMap(pos => circuits.filter(_(pos))).toSet match {
            case matchingCircuits if matchingCircuits.size == 2 => ((circuits diff matchingCircuits) + matchingCircuits.flatten, noOfConnections + 1)
            case matchingCircuits if matchingCircuits.size == 1 =>
              if (circuits.exists(circuit => connection.forall(circuit(_)))) (circuits, noOfConnections)
              else (circuits.map(circuit => if (connection.exists(circuit(_))) circuit ++ connection else circuit), noOfConnections + 1)
            case _ => (circuits + connection.toSet, noOfConnections + 1)
          }
          if (newNoOfConnections != noOfConnections)
            logger.debug(
              s"***\n- $newNoOfConnections \n${newCircuits.mkString("\n")}\n --- $connection\n --- ${circuits.exists(circuit => connection.forall(circuit(_)))}\n --- ${connection.flatMap(pos => circuits.filter(_(pos))).length}\n***"
            )
          val newAns = if (newNoOfConnections == noOfPositions - 1) Some(connection.map(_._1).product) else ans
          (newCircuits, newNoOfConnections, newAns)
        case (acc, _) => acc
      }
      ._3
      .get
  }
}
