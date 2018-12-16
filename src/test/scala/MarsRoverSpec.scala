import MarsRover.Command.{Forwards, RotateAntiClockwise, RotateClockwise}
import MarsRover.Orientation._
import MarsRover._
import org.scalatest.Matchers
import org.scalatest.FlatSpec

//  y
//  4|    e
//  3|
//  2|        // e is (3,2) limit is (5,5)
//  1|__m m____
//    1 2 3 4 x
// assume start is always (1,1)

class MarsRoverSpec extends FlatSpec with Matchers {

  "The Rover" should "move forwards" in {
    val instructions = Rover(GridSquare(3,2), Right, Nil, (5,5), List.empty).forwards
    instructions shouldEqual Rover(GridSquare(4,2), Right, List(Forwards), (5,5), List.empty)
    instructions.position.xAxis shouldEqual 4
    instructions.position.yAxis shouldEqual 2
  }

  it should "move forwards over the edge" in {
    val result = Rover(GridSquare(2,1), Down, Nil, (4,4), List.empty).forwards
    result shouldEqual Rover(GridSquare(2,4), Down, List(Forwards), (4,4), List.empty)

  }

  it should "turn right" in {
    val result = Rover(GridSquare(1,1), Right, Nil, (2,2), List.empty).rotateClockwise
    result shouldEqual Rover(GridSquare(1,1), Down, List(RotateClockwise), (2,2), List.empty)
  }

  it should "turn left" in {
    val result = Rover(GridSquare(1,1), Right, Nil, (2,2), List.empty).rotateAntiClockwise
    result shouldEqual Rover(GridSquare(1,1), Up, List(RotateAntiClockwise), (2,2), List.empty)
  }

  it should "execute a series of moves" in {
    val result = Rover(GridSquare(1,1), Right, Nil, (2,2), List.empty).forwards.forwards.forwards.rotateAntiClockwise
    result.moves shouldEqual List(Forwards, Forwards, Forwards, RotateAntiClockwise)
  }

  it should "take a coordinate and turn it into a move" in {
    val coords = List(GridSquare(3,2))
    val result = Rover(GridSquare(2,2), Up, List.empty, (3,3), List.empty).executeMoves(coords)
    result.moves shouldBe List(RotateClockwise, Forwards)
  }

  it should "take a list of coordinates and turn it into moves" in {
    val coords = List(GridSquare(3,2), GridSquare(3,3), GridSquare(3,4))
    val result = Rover(GridSquare(2,2), Up, List.empty, (8,8), List.empty).executeMoves(coords)
    result.moves shouldBe List(RotateClockwise, Forwards, RotateAntiClockwise, Forwards, Forwards)
  }

  it should "avoid mountains" in {
    val path = Pathfinder.shortestPath(4, GridSquare(1,1), GridSquare(2,4))
    val rover = Rover(GridSquare(1,1), Right, List.empty, (4,4), List(GridSquare(2,1)))
    rover.tryAllPaths(path).moves shouldBe List(RotateClockwise, Forwards, RotateAntiClockwise, Forwards)
  }

  "The Pathfinder" should "find the shortest path" in {
    val path = Pathfinder.shortestPath(6, GridSquare(1,1), GridSquare(2,3))

    path shouldBe List(
      List(GridSquare(2,1), GridSquare(2,2), GridSquare(2,3)), List(GridSquare(1,2), GridSquare(1,3), GridSquare(2,3)),
      List(GridSquare(6,1), GridSquare(5,1), GridSquare(4,1), GridSquare(3,1), GridSquare(2,1), GridSquare(2,6),
        GridSquare(2,5), GridSquare(2,4), GridSquare(2,3)), List(GridSquare(1,6), GridSquare(1,5), GridSquare(1,4),
        GridSquare(1,3), GridSquare(6,3), GridSquare(5,3), GridSquare(4,3), GridSquare(3,3), GridSquare(2,3)),
      List(GridSquare(2,1), GridSquare(2,6), GridSquare(2,5), GridSquare(2,4), GridSquare(2,3)),
      List(GridSquare(6,1), GridSquare(5,1), GridSquare(4,1), GridSquare(3,1), GridSquare(2,1),
        GridSquare(2,2), GridSquare(2,3)),
      List(GridSquare(1,2), GridSquare(1,3), GridSquare(6,3), GridSquare(5,3), GridSquare(4,3),
        GridSquare(3,3), GridSquare(2,3)),
      List(GridSquare(1,6), GridSquare(1,5), GridSquare(1,4), GridSquare(1,3), GridSquare(2,3)))
  }

  it should "go over the edges if it's the shortest path" in {
    val path = Pathfinder.shortestPath(4, GridSquare(1,1), GridSquare(2,4))

    path shouldBe List(
      List(GridSquare(2,1), GridSquare(2,2), GridSquare(2,3), GridSquare(2,4)), List(GridSquare(1,2),
        GridSquare(1,3), GridSquare(1,4), GridSquare(2,4)), List(GridSquare(4,1), GridSquare(3,1),
        GridSquare(2,1), GridSquare(2,4)),
      List(GridSquare(1,4), GridSquare(4,4), GridSquare(3,4), GridSquare(2,4)),
      List(GridSquare(2,1), GridSquare(2,4)),
      List(GridSquare(4,1), GridSquare(3,1), GridSquare(2,1), GridSquare(2,2), GridSquare(2,3),
        GridSquare(2,4)),
      List(GridSquare(1,2), GridSquare(1,3), GridSquare(1,4), GridSquare(4,4), GridSquare(3,4), GridSquare(2,4)),
      List(GridSquare(1,4), GridSquare(2,4)))
  }

  it should "find a best path and make rover move" in {
    val path = Pathfinder.shortestPath(4, GridSquare(1,1), GridSquare(3,4))
    val mountains = List(GridSquare(2,1), GridSquare(3,1))
    val rover = Rover(GridSquare(1,1), Right, List.empty, (4,4), mountains)


    rover.tryAllPaths(path) shouldBe
      Rover(
        GridSquare(3,4),
        Left,
        List(RotateClockwise, Forwards, RotateClockwise, Forwards, Forwards),
        (4,4),
        mountains
      )
  }
}

