package puzzles.day6

object Day6Part1 {
  def getAns(input: List[String]): Long = {
    val problem = input.map(_.split(' ').filter(_.nonEmpty).toVector).toVector
    problem.transpose.foldLeft(0L) { case (acc, numbers :+ symbol) =>
      if (symbol == "+") acc + numbers.map(_.toLong).sum
      else acc + numbers.map(_.toLong).product
    }
  }
}
