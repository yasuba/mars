import MarsRover.GridSquare

object Pathfinder {
  def incrementX(first: Boolean, range: Range, start: GridSquare, end: GridSquare): List[(Int, Int)] =
    (for {
      xCoord <- range
      yCoord = if (first) start.yAxis else end.yAxis
    } yield (xCoord, yCoord)
      ).toList

  def incrementY(first: Boolean, range: Range, start: GridSquare, end: GridSquare): List[(Int, Int)] =
    (for {
      yCoord <- range
      xCoord = if (first) start.xAxis else end.xAxis
    } yield (xCoord, yCoord)
      ).toList

  private def removeStart(result: List[(Int, Int)], start: GridSquare): List[(Int, Int)] =
    result.distinct.filterNot(_ == (start.xAxis, start.yAxis))

  def createPath(firstRange: Range, secondRange: Range, xFirst: Boolean, yFirst: Boolean): GridSquare => GridSquare => List[GridSquare] = start => end =>  {
    val xTuples = incrementX(xFirst, firstRange, start, end)
    val yTuples = incrementY(yFirst, secondRange, start, end)
    val concatTuples: List[(Int, Int)] = if (xFirst) xTuples ++ yTuples else yTuples ++ xTuples
    removeStart(concatTuples, start).map(coord => GridSquare(coord._1, coord._2))
  }

  def createPaths(firstPositiveRange: Range,
                  secondPositiveRange:Range,
                  firstNegativeRange: Range,
                  secondNegativeRange: Range) = {
    List(
      createPath(firstPositiveRange, secondPositiveRange, true, false),
      createPath(firstPositiveRange, secondPositiveRange, false, true),
      createPath(firstNegativeRange, secondNegativeRange, true, false),
      createPath(firstNegativeRange, secondNegativeRange, false, true),
      createPath(firstPositiveRange, secondNegativeRange, true, false),
      createPath(firstNegativeRange, secondPositiveRange, true, false),
      createPath(firstNegativeRange, secondPositiveRange, false, true),
      createPath(firstPositiveRange, secondNegativeRange, false, true)
    )
  }

  def shortestPath(limit: Int, start: GridSquare, end: GridSquare): List[List[GridSquare]] = {
    val firstPositiveRange  = start.xAxis to end.xAxis
    val secondPositiveRange = start.yAxis to end.yAxis
    val firstNegativeRange  = (end.xAxis to limit).reverse
    val secondNegativeRange = (end.yAxis to limit).reverse

    createPaths(firstPositiveRange, secondPositiveRange, firstNegativeRange, secondNegativeRange) map (f => f(start)(end))
  }
}
