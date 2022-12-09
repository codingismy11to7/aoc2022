package aoc

import com.softwaremill.quicklens._
import zio.Chunk

import scala.annotation.tailrec

object Day9 extends AdventDay {
  override final val day          = 9
  override def numberOfTests: Int = 2

  sealed trait Direction
  case object Right extends Direction
  case object Left  extends Direction
  case object Up    extends Direction
  case object Down  extends Direction

  private final case class Move(dir: Direction, count: Int) {
    def applyTo(state: State): State =
      (1 to count).foldLeft(state) { case (prevState, _) =>
        val knots = prevState.knots
        val newHeadPos = dir match {
          case Right => knots.head.modify(_.deltX)(_ + 1)
          case Left  => knots.head.modify(_.deltX)(_ - 1)
          case Up    => knots.head.modify(_.deltY)(_ - 1)
          case Down  => knots.head.modify(_.deltY)(_ + 1)
        }

        @tailrec
        def loop(
            prevPos: Position = newHeadPos,
            rem: List[Position] = knots.tail,
            acc: List[Position] = Nil,
        ): (List[Position], Position) =
          rem match {
            case Nil => acc.reverse -> prevPos
            case currPos :: rest =>
              val newPosForCurr = newBPosAfterFollowingAMove(prevPos, currPos)
              loop(newPosForCurr, rest, newPosForCurr :: acc)
          }
        val (newPositions, tailVisited) = loop()
        prevState.copy(knots = newHeadPos :: newPositions).modify(_.visited)(_ + tailVisited)
      }
  }
  private object Move {
    private val right = """R (\d+)""".r
    private val left  = """L (\d+)""".r
    private val up    = """U (\d+)""".r
    private val down  = """D (\d+)""".r
    def apply(line: String): Move = line match {
      case right(num) => Move(Right, num.toInt)
      case left(num)  => Move(Left, num.toInt)
      case up(num)    => Move(Up, num.toInt)
      case down(num)  => Move(Down, num.toInt)
      case _          => sys.error(s"invalid line $line")
    }
  }

  private final case class Position(deltX: Int, deltY: Int)

  private def touching(aPos: Position, bPos: Position) =
    math.abs(aPos.deltX - bPos.deltX) <= 1 && math.abs(aPos.deltY - bPos.deltY) <= 1

  private def newBPosAfterFollowingAMove(aPos: Position, bPos: Position) =
    if (touching(aPos, bPos)) bPos
    else if (aPos.deltY == bPos.deltY)
      bPos.modify(_.deltX).setTo((aPos.deltX + bPos.deltX) / 2)
    else if (aPos.deltX == bPos.deltX)
      bPos.modify(_.deltY).setTo((aPos.deltY + bPos.deltY) / 2)
    else {
      if (bPos.deltX < aPos.deltX && bPos.deltY < aPos.deltY)
        bPos.modify(_.deltX)(_ + 1).modify(_.deltY)(_ + 1)
      else if (bPos.deltX > aPos.deltX && bPos.deltY < aPos.deltY)
        bPos.modify(_.deltX)(_ - 1).modify(_.deltY)(_ + 1)

      val newX = bPos.deltX + (if (bPos.deltX < aPos.deltX) 1 else -1)
      val newY = bPos.deltY + (if (bPos.deltY < aPos.deltY) 1 else -1)
      Position(newX, newY)
    }

  private final case class State(knots: List[Position], visited: Set[Position])
  private object State { def create(numKnots: Int): State = State(List.fill(numKnots)(Position(0, 0)), Set.empty) }

  private def app(dataFile: String)(numKnots: Int) = resourceLines(dataFile)
    .runFold(State.create(numKnots)) { case (state, line) =>
      Move(line).applyTo(state)
    }
    .map(_.visited.size)

  override def part1TestExpectation: Any           = Chunk(13, 88)
  override def part1Expectation: Any               = 6_745
  override def part1(dataFile: String): STask[Any] = app(dataFile)(2)

  override def part2TestExpectation: Any           = Chunk(1, 36)
  override def part2Expectation: Any               = 2_793
  override def part2(dataFile: String): STask[Any] = app(dataFile)(10)
}
