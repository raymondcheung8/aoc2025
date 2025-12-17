package puzzles.day4

import com.typesafe.scalalogging.StrictLogging

object Day4Part1 extends StrictLogging {
  def getAnsInner(input: Vector[(Vector[(Char, Int)], Int)]): Int = {
    input.foldLeft(0) { case (count, (row, j)) =>
      count + row.foldLeft(0) { case (rowCount, (isRoll, i)) =>
        lazy val isAccessible =
          (j - 1 to j + 1)
            .collect {
              case j2 if j2 >= 0 && j2 < input.length =>
                (i - 1 to i + 1).collect {
                  case i2 if i2 >= 0 && i2 < row.length && !(i == i2 && j == j2) && input(j2)._1(i2)._1 == '@' =>
                    logger.debug(s"($i, $j) --- ($i2, $j2)")
                    1
                }
            }
            .flatten
            .sum < 4
        if (isRoll == '@' && isAccessible) rowCount + 1 else rowCount
      }
    }
  }

  def getAns(input: List[String]): Int = getAnsInner(input.map(_.zipWithIndex.toVector).zipWithIndex.toVector)
}
