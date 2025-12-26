package puzzles.day9

import com.typesafe.scalalogging.StrictLogging

object Day9Part2 extends StrictLogging {
  private def area(pair: List[(Long, Long)]): Long = pair match {
    case List((x, y), (x2, y2), _*) => (math.abs(x - x2) + 1) * (math.abs(y - y2) + 1)
  }

  private def getConstraint(x: Long, y: Long, x2: Long, y2: Long): (Option[(Long, Long, Long)], Option[(Long, Long, Long)]) =
    if (x == x2) (Some((x, y.min(y2), y.max(y2))), None) else (None, Some((y, x.min(x2), x.max(x2))))

  private def isValidPoint(xConstraints: List[(Long, Long, Long)], yConstraints: List[(Long, Long, Long)], x: Double, y: Double): Boolean =
    xConstraints.exists { case (x2, yMin, yMax) => x >= x2 && y >= yMin && y <= yMax } &&
      xConstraints.exists { case (x2, yMin, yMax) => x <= x2 && y >= yMin && y <= yMax } &&
      yConstraints.exists { case (y2, xMin, xMax) => y >= y2 && x >= xMin && x <= xMax } &&
      yConstraints.exists { case (y2, xMin, xMax) => y <= y2 && x >= xMin && x <= xMax }

  private def isValidSide(
      xConstraints: List[(Long, Long, Long)],
      yConstraints: List[(Long, Long, Long)],
      x1: Long,
      y1: Long,
      x2: Long,
      y2: Long,
      otherXorY: Long
  ) =
    isValidPoint(xConstraints, yConstraints, (x1 + x2).toDouble / 2.0, (y1 + y2).toDouble / 2.0) &&
      (x1 == x2 && !yConstraints.exists { case (y, xMin, xMax) =>
        y > y1.min(y2) && y < y1.max(y2) && (x1.min(otherXorY) <= xMin && x1.max(otherXorY) >= xMax || x1.min(otherXorY) >= xMin && x1.max(otherXorY) <= xMax)
      } ||
        y1 == y2 && !xConstraints.exists { case (x, yMin, yMax) =>
          x > x1.min(x2) && x < x1.max(x2) && (y1.min(otherXorY) <= yMin && y1.max(otherXorY) >= yMax || y1.min(otherXorY) >= yMin && y1.max(otherXorY) <= yMax)
        })

  def getAns(input: List[String]): Long = {
    val positions                       = input.map(_.split(',') match { case Array(x, y, _*) => (x.toLong, y.toLong) })
    val positionsSet                    = positions.toSet
    val (_, xConstraints, yConstraints) = positions
      .foldLeft((positions.last, List.empty[(Long, Long, Long)], List.empty[(Long, Long, Long)])) { case (((x, y), xConstraints, yConstraints), (x2, y2)) =>
        val (xConstraint, yConstraint) = getConstraint(x, y, x2, y2)
        ((x2, y2), xConstraint.toList ::: xConstraints, yConstraint.toList ::: yConstraints)
      }
    positions
      .combinations(2)
      .map { corners =>
        logger.debug(s"unfiltered: $corners --- ${area(corners)}")
        corners
      }
      .filter { existingCorners =>
        val List((x1, y1), (x2, y2), _*) = existingCorners
        val newCorners                   = List((x1, y2), (x2, y1))
        val areCornersValid              = newCorners.forall { case (x, y) => positionsSet((x, y)) || isValidPoint(xConstraints, yConstraints, x, y) }
        lazy val isRectangleValid        =
          isValidSide(xConstraints, yConstraints, x1, y1, x1, y2, x2) &&
            isValidSide(xConstraints, yConstraints, x1, y1, x2, y1, y2) &&
            isValidSide(xConstraints, yConstraints, x2, y2, x1, y2, y1) &&
            isValidSide(xConstraints, yConstraints, x2, y2, x2, y1, x1)
        areCornersValid && isRectangleValid
      }
      .map { corners =>
        logger.debug(s"filtered: $corners --- ${area(corners)}")
        corners
      }
      .map(area)
      .max
  }
}
