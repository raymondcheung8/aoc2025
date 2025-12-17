package puzzles.day5

import com.typesafe.scalalogging.StrictLogging

object Day5Part2 extends StrictLogging {
  def getAns(input: List[String]): Long = {
    val freshRanges =
      input.collect {
        case str if str.contains('-') =>
          str.split('-') match {
            case Array(l, r, _*) => (l.toLong, r.toLong)
          }
      }

    val freshRangesNoOverlap = freshRanges.tail.foldLeft(freshRanges.head :: Nil) { case (ranges, lr) =>
      logger.debug(s"$ranges + $lr")
      val finalRanges = ranges.foldLeft(lr :: Nil) { case (newRanges, (l2, r2)) =>
        newRanges.flatMap { case (l, r) =>
          val isLeft   = (x: Long) => x < l2 && x < r2
          val isInside = (x: Long) => x >= l2 && x <= r2
          val isRight  = (x: Long) => x > l2 && x > r2

          (l, r) match {
            case (l, r) if isInside(l) && isInside(r)                         => Nil
            case (l, r) if isLeft(l) && isLeft(r) || isRight(l) && isRight(r) => (l, r) :: Nil
            case (l, r) if isLeft(l) && isInside(r)                           => (l, l2 - 1) :: Nil
            case (l, r) if isInside(l) && isRight(r)                          => (r2 + 1, r) :: Nil
            case (l, r) if isLeft(l) && isRight(r)                            => (l, l2 - 1) :: (r2 + 1, r) :: Nil
          }
        }
      } ::: ranges
      logger.debug(s"$finalRanges")
      finalRanges
    }

    freshRangesNoOverlap.map { case (l, r) => r - l + 1 }.sum
  }
}
