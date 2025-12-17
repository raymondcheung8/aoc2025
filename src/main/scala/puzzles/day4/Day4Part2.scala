package puzzles.day4

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

object Day4Part2 extends StrictLogging {
  @tailrec
  def getAnsInner(input: Vector[(Vector[(Char, Int)], Int)], count: Int = 0): Int = {
    val (newCount, newInput) =
      input.foldLeft((0, Vector[(Vector[(Char, Int)], Int)]())) { case ((count, newInput), (row, j)) =>
        val (newRowCount, newRowInput) = row.foldLeft((0, Vector[(Char, Int)]())) { case ((rowCount, newRow), (char, i)) =>
          lazy val isAccessible =
            (j - 1 to j + 1)
              .collect {
                case j2 if j2 >= 0 && j2 < input.length =>
                  (i - 1 to i + 1).collect {
                    case i2 if i2 >= 0 && i2 < row.length && !(i == i2 && j == j2) && input(j2)._1(i2)._1 == '@' => 1
                  }
              }
              .flatten
              .sum < 4
          if (char == '@' && isAccessible) (rowCount + 1, newRow :+ ('.', i)) else (rowCount, newRow :+ (char, i))
        }
        (count + newRowCount, newInput :+ (newRowInput, j))
      }
    logger.debug(s"$newCount --- $newInput")
    if (newCount == 0) count else getAnsInner(newInput, newCount + count)
  }

  def getAns(input: List[String]): Int = getAnsInner(input.map(_.zipWithIndex.toVector).zipWithIndex.toVector)
}
