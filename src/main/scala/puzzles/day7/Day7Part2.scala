package puzzles.day7

object Day7Part2 {
  private def traverseTimeline(input: List[String], i: Int, j: Int, cache: Map[(Int, Int), Long] = Map()): (Long, Map[(Int, Int), Long]) = input match {
    case h :: t if h(i) == '^' =>
      cache.get((i, j)) match {
        case Some(count) => (count, cache)
        case None        =>
          val (leftCount, leftCache)   = traverseTimeline(t, i - 1, j + 1, cache)
          val (rightCount, rightCache) = traverseTimeline(t, i + 1, j + 1, leftCache)
          val newCount                 = leftCount + rightCount
          val newCache                 = rightCache + ((i, j) -> newCount)
          (newCount, newCache)
      }
    case _ :: t => traverseTimeline(t, i, j + 1, cache)
    case Nil    => (1L, cache)
  }

  def getAns(input: List[String]): Long = {
    val startIndex = input.head.indexOf('S')
    traverseTimeline(input.tail, startIndex, 1)._1
  }
}
