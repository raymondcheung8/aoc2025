package puzzles.day9

object Day9Part1 {
  private def area(pair: List[(Long, Long)]): Long = pair match {
    case List((x, y), (x2, y2), _*) => math.abs(x - x2 + 1) * math.abs(y - y2 + 1)
  }

  def getAns(input: List[String]): Long = {
    val positions     = input.map(_.split(',') match { case Array(x, y, _*) => (x.toLong, y.toLong) })
    positions.combinations(2).map(area).max
  }
}
