import MarsRover.{GridSquare, Rover}
import MarsRover.Orientation._

object Main extends App {
  val gridSize: String = scala.io.StdIn.readLine("What size grid do you want? (e.g. 2) ")

  val xAxis = gridSize.toInt

  val limit = (xAxis, xAxis)

  val endPoint = scala.io.StdIn.readLine("What are the coordinates of the Rover's destination? Enter them in the following format: 2,2 ")

  val destination: (Int, Int) = (endPoint.split(",").headOption.getOrElse("").toInt, endPoint.split(",").reverse.headOption.getOrElse("").toInt)

  val paths = Pathfinder.shortestPath(limit._1, GridSquare(1,1), GridSquare(destination._1, destination._2))

  val rover = Rover(GridSquare(1,1), Right, List(), limit, List.empty).tryAllPaths(paths)

  System.out.println("The rover is starting on (1,1) facing Right")
  System.out.println("The Mars Rover's instructions are: ")

  println(rover.moves)
}

