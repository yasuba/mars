import MarsRover.GridSquare

object Pathfinder {
  def zigZagPath(xFirst: Boolean, end: GridSquare): List[(Int, Int)] = {
    val paths = for {
        coord      <- 1 to end.xAxis
        coordPlus1 = if (coord + 1 >= end.xAxis) end.xAxis else coord + 1
      } yield {
      if (xFirst) List((coordPlus1, coord), (coordPlus1, coordPlus1))
      else List((coord, coordPlus1), (coordPlus1, coordPlus1))
    }
    paths.toList.flatten
  }

  def zigZagReverse(xFirst: Boolean, end: GridSquare, limit: Int): List[(Int, Int)] = {
    val paths = for {
      coord       <- 1 to end.xAxis
      reverseCoord <- (1 to end.xAxis).reverse
      coordMinus1 = if (coord - 1 <= 1) limit else coord - 1
    } yield {
      if (xFirst) List((coordMinus1, coord), (coordMinus1, coordMinus1))
      else List((coord, coordMinus1), (coordMinus1, coordMinus1))
    }
    paths.toList.flatten
  }

  def increment(first: Boolean, range: Range, incX: Boolean, start: GridSquare, end: GridSquare): List[(Int, Int)] =
    (for {
      coord <- range
      xCoord = if (first) start.xAxis else end.xAxis
      yCoord = if (first) start.yAxis else end.yAxis
    } yield
      if (incX) (coord, yCoord) else (xCoord, coord)
      ).toList

  private def removeStart(result: List[(Int, Int)], start: GridSquare): List[(Int, Int)] =
    result.distinct.filterNot(_ == (start.xAxis, start.yAxis))

  def createPath(xRange: Range, yRange: Range, xFirst: Boolean): GridSquare => GridSquare => List[GridSquare] = start => end =>  {
    val xTuples = increment(xFirst, xRange, incX = true, start, end)
    val yTuples = increment(!xFirst, yRange, incX = false, start, end)
    val concatTuples: List[(Int, Int)] = if (xFirst) xTuples ++ yTuples else yTuples ++ xTuples
    removeStart(concatTuples, start).map(coord => GridSquare(coord._1, coord._2))
  }

  def createPaths(xPositiveRange: Range,
                  yPositiveRange: Range,
                  xNegativeRange: Range,
                  yNegativeRange: Range): List[GridSquare => GridSquare => List[GridSquare]] = {
    List(
      createPath(xPositiveRange, yPositiveRange, xFirst = true),
      createPath(xPositiveRange, yPositiveRange, xFirst = false),
      createPath(xNegativeRange, yNegativeRange, xFirst = true),
      createPath(xNegativeRange, yNegativeRange, xFirst = false),
      createPath(xPositiveRange, yNegativeRange, xFirst = true),
      createPath(xNegativeRange, yPositiveRange, xFirst = true),
      createPath(xNegativeRange, yPositiveRange, xFirst = false),
      createPath(xPositiveRange, yNegativeRange, xFirst = false)
    )

  }

  def shortestPath(limit: Int, start: GridSquare, end: GridSquare): List[List[GridSquare]] = {
    createPaths(
      start.xAxis to end.xAxis,
      start.yAxis to end.yAxis,
      (end.xAxis to limit).reverse,
      (end.yAxis to limit).reverse
    ) map (path => path(start)(end))
  }
}
