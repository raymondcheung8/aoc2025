package puzzles.day6

import com.typesafe.scalalogging.StrictLogging

object Day6Part2 extends StrictLogging {
  def getAns(input: List[String]): Long = {
    val problem           = input.filter(_.nonEmpty).map(_.toVector).toVector
    val transposedProblem = problem.transpose
    transposedProblem
      .foldLeft((0L, (List[Long](), ' '), 0)) { case ((acc, (numbers, currentSymbol), count), digits :+ newSymbol) =>
        val newNumber = digits.filter(_.isDigit).mkString
        if (newNumber.isEmpty && newSymbol == ' ') {
          val result = if (currentSymbol == '+') numbers.sum else numbers.product
          logger.debug(s"$numbers --- $currentSymbol --- $result")
          (acc + result, (Nil, ' '), count + 1)
        } else {
          val symbol = if (newSymbol == ' ') currentSymbol else newSymbol
          if (count == transposedProblem.length - 1) {
            val result = if (symbol == '+') (newNumber.toLong :: numbers).sum else numbers.product
            logger.debug(s"$numbers --- $currentSymbol --- $result")
            (acc + result, (Nil, ' '), count + 1)
          } else (acc, (newNumber.toLong :: numbers, symbol), count + 1)
        }
      }
      ._1
  }
}
