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

  def createPositiveRange(startAxis: Int, endAxis: Int): Range =
    startAxis to endAxis

  def createNegativeRange(endAxis: Int, limitYaxis: Int): Range =
    (endAxis to limitYaxis).reverse

  def createPath(firstRange: Range, secondRange: Range, start: GridSquare, end: GridSquare, xFirst: Boolean, yFirst: Boolean): List[(Int, Int)] = {
    val xTuples = incrementX(xFirst, firstRange, start, end)
    val yTuples = incrementY(yFirst, secondRange, start, end)
    val concatTuples: List[(Int, Int)] = if (xFirst) xTuples ++ yTuples else yTuples ++ xTuples
    removeStart(concatTuples, start)
  }

  def shortestPath(start: GridSquare, end: GridSquare, limit: (Int, Int)): List[List[GridSquare]] = {
    val firstPositiveRange  = createPositiveRange(start.xAxis, end.xAxis)
    val secondPositiveRange = createPositiveRange(start.yAxis, end.yAxis)
    val firstNegativeRange  = createNegativeRange(end.xAxis, limit._1)
    val secondNegativeRange = createNegativeRange(end.yAxis, limit._2)

    val xFirstPositive: List[(Int, Int)] = createPath(firstPositiveRange, secondPositiveRange, start, end, true, false)
    val yFirstPositive: List[(Int, Int)] = createPath(firstPositiveRange, secondPositiveRange, start, end, false, true)
    val xFirstNegative: List[(Int, Int)] = createPath(firstNegativeRange, secondNegativeRange, start, end, true, false)
    val yFirstNegative: List[(Int, Int)] = createPath(firstNegativeRange, secondNegativeRange, start, end, false, true)
    val xFirstPosYNeg: List[(Int, Int)] = createPath(firstPositiveRange, secondNegativeRange, start, end, true, false)
    val xFirstNegYPos: List[(Int, Int)] = createPath(firstNegativeRange, secondPositiveRange, start, end, true, false)
    val yFirstPosXNeg: List[(Int, Int)] = createPath(firstNegativeRange, secondPositiveRange, start, end, false, true)
    val yFirstNegXPos: List[(Int, Int)] = createPath(firstPositiveRange, secondNegativeRange, start, end, false, true)

    List(
      xFirstPositive,
      yFirstPositive,
      xFirstNegative,
      yFirstNegative,
      xFirstPosYNeg,
      xFirstNegYPos,
      yFirstPosXNeg,
      yFirstNegXPos
    ).distinct.map(pathToGridSquares)
  }

  def pathToGridSquares(path: List[(Int, Int)]): List[GridSquare] = {
    path map (coord => GridSquare(coord._1, coord._2))
  }
}