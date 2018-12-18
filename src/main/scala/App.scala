import MarsRover.Orientation._
import MarsRover.{GridSquare, Orientation, Rover}

import scala.annotation.tailrec

object Main extends App {
  val gridSize: String = scala.io.StdIn.readLine("What size grid do you want? (e.g. 2) ")

  val xAxis = gridSize.toInt

  val limit = (xAxis, xAxis)

  val endPoint = scala.io.StdIn.readLine("What are the coordinates of the Rover's destination? Enter them in the following format: 2,2 ")

  val destination: GridSquare = GridSquare(endPoint.split(",").headOption.getOrElse("").toInt, endPoint.split(",").reverse.headOption.getOrElse("").toInt)

  lazy val direction = scala.io.StdIn.readLine("Which direction is the Rover facing? Up, down, left or right?")

  val paths = Pathfinder.shortestPath(limit._1, GridSquare(1,1), destination)

  @tailrec
  def orientation(direction: String): Orientation = direction.toLowerCase match {
    case "up"    => Up
    case "down"  => Down
    case "left"  => Left
    case "right" => Right
    case _       => orientation(scala.io.StdIn.readLine("Direction not recognised, please try again"))
  }

  val rover = Rover(GridSquare(1,1), orientation(direction), List(), limit, destination, List.empty).tryAllPaths(paths)

  System.out.println(s"The rover is starting on (1,1) facing $direction")
  System.out.println("The Mars Rover's instructions are: ")

  println(rover.moves)
}

