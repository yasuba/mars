package MarsRover

import MarsRover.Command.{Forwards, RotateAntiClockwise, RotateClockwise}
import MarsRover.Orientation._

import scala.annotation.tailrec

case class GridSquare(xAxis: Int, yAxis: Int)

sealed trait Command
object Command {
  object Forwards extends Command {
    override def toString(): String = "Forwards"
  }
  object RotateClockwise extends Command {
    override def toString(): String = "Rotate Clockwise"
  }
  object RotateAntiClockwise extends Command {
    override def toString(): String = "Rotate AntiClockwise"
  }
}

sealed trait Orientation
object Orientation {
  object Right extends Orientation {
    override def toString(): String = "Right"
  }
  object Left extends Orientation {
    override def toString(): String = "Left"
  }
  object Down extends Orientation {
    override def toString(): String = "Down"
  }
  object Up extends Orientation {
    override def toString(): String = "Up"
  }
}

case class Rover(position: GridSquare,
                 orientation: Orientation,
                 moves: List[Command],
                 limit: (Int, Int),
                 destination: GridSquare,
                 mountains: List[GridSquare]) {

  def forwards: Rover = {
    orientation match {
      case Right =>
        val xAxis = if (position.xAxis + 1 > limit._1) 1 else position.xAxis + 1
        checkForMountains(GridSquare(xAxis, position.yAxis))

      case Up =>
        val yAxis = if (position.yAxis + 1 > limit._2) 1 else position.yAxis + 1
        checkForMountains(GridSquare(position.xAxis, yAxis))

      case Left =>
        val xAxis = if (position.xAxis - 1 < 1) limit._2 else position.xAxis - 1
        checkForMountains(GridSquare(xAxis, position.yAxis))

      case Down =>
        val yAxis = if (position.yAxis - 1 < 1) limit._2 else position.yAxis - 1
        checkForMountains(GridSquare(position.xAxis, yAxis))
    }
  }

  def checkForMountains(newPosition: GridSquare): Rover =
    if (mountains.contains(newPosition)) this
    else Rover(newPosition, orientation, moves :+ Forwards, limit, destination, mountains)

  def rotateClockwise: Rover =
    orientation match {
      case Right => Rover(position, Down, moves :+ RotateClockwise, limit, destination, mountains)
      case Left => Rover(position, Up, moves :+ RotateClockwise, limit, destination, mountains)
      case Up => Rover(position, Right, moves :+ RotateClockwise, limit, destination, mountains)
      case Down => Rover(position, Left, moves :+ RotateClockwise, limit, destination, mountains)
    }

  def rotateAntiClockwise: Rover =
    orientation match {
      case Right => Rover(position, Up, moves :+ RotateAntiClockwise, limit, destination, mountains)
      case Left => Rover(position, Down, moves :+ RotateAntiClockwise, limit, destination, mountains)
      case Up => Rover(position, Left, moves :+ RotateAntiClockwise, limit, destination, mountains)
      case Down => Rover(position, Right, moves :+ RotateAntiClockwise, limit, destination, mountains)
    }

  def coordToCommand(coord: GridSquare, rover: Rover): Rover =
    if (rover.forwards.position == coord) rover.forwards
    else if (rover.rotateClockwise.forwards.position == coord) rover.rotateClockwise.forwards
    else if (rover.rotateAntiClockwise.forwards.position == coord) rover.rotateAntiClockwise.forwards
    else if (rover.rotateClockwise.rotateClockwise.forwards.position == coord) rover.rotateClockwise.rotateClockwise.forwards
    else rover.rotateAntiClockwise.rotateAntiClockwise.forwards

  def tryAllPaths(paths: List[List[GridSquare]]): Rover = {
    val rovers = paths.map(executeMoves)
    rovers.minBy(_.moves.length)
  }

  def executeMoves(coords: List[GridSquare]): Rover = {
    @tailrec
    def loop(coords: List[GridSquare], rover: Rover): Rover =
      coords match {
        case Nil      => this
        case h :: Nil => coordToCommand(h, rover)
        case h :: t   => loop(t, coordToCommand(h, rover))
      }
    loop(coords, this)
  }
}




