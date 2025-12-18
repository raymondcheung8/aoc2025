package puzzles.day7

object Day7Part1 {
  def getAns(input: List[String]): Int = {
    val startIndex = input.head.indexOf('S')
    input.tail.foldLeft((0, Set(startIndex))) { case ((count, indices), row) =>
      indices.foldLeft((count, indices)) { case ((newCount, newIndices), i) =>
        if (row(i) == '^') (newCount + 1, newIndices - i + (i - 1) + (i + 1))
        else (newCount, newIndices)
      }
    }._1
  }
}
