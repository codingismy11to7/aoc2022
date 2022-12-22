package aoc

import enumeratum._
import zio.Chunk

import scala.annotation.tailrec

object Day22 extends AdventDay {
  override final val day = 22

  private def loopNeighbor[A](items: IndexedSeq[A], idx: Int, positive: Boolean) =
    items(((idx + items.size) + (if (positive) 1 else -1)) % items.size)

  private sealed trait Spot
  private sealed trait RealSpot extends Spot
  private object Spot {
    case object NotASpot extends Spot
    case object Empty    extends RealSpot
    case object Wall     extends RealSpot

    final val charMap = Map(' ' -> NotASpot, '.' -> Empty, '#' -> Wall)
  }

  private sealed trait Turn extends EnumEntry
  private object Turn extends Enum[Turn] {
    override val values: IndexedSeq[Turn] = findValues
    case object Left  extends Turn
    case object Right extends Turn

    final val charMap = Map('L' -> Left, 'R' -> Right)
  }

  private sealed trait Facing extends EnumEntry {
    def ord: Int                 = Facing.values.indexOf(this)
    def turn(turn: Turn): Facing = loopNeighbor(Facing.values, ord, turn == Turn.Right)
  }
  private object Facing extends Enum[Facing] {
    override def values: IndexedSeq[Facing] = findValues
    case object Right extends Facing
    case object Down  extends Facing
    case object Left  extends Facing
    case object Up    extends Facing
  }

  private sealed trait Instr
  private object Instr {
    final case class Move(n: Int)        extends Instr
    final case class Turn(t: Day22.Turn) extends Instr

    def parseInstrs(line: String): Chunk[Instr] = {
      import fastparse._
      import NoWhitespace._
      def digits(implicit ev: P[Any])   = P(CharsWhileIn("0-9"))
      def integral(implicit ev: P[Any]) = P(CharIn("1-9") ~ digits.?)
      def move(implicit ev: P[Any])     = P(integral).!.map(_.toInt).map(Instr.Move)
      def turn(implicit ev: P[Any])     = P(CharIn("LR")).!.map(_.head).map(Day22.Turn.charMap(_)).map(Turn)

      def turnAndMove(implicit ev: P[Any]) = P(turn ~ move).map(t => Chunk(t._1, t._2))
      def whole(implicit ev: P[Any])       = P(move ~ turnAndMove.rep).map(t => t._2.foldLeft(Chunk[Instr](t._1))(_ ++ _))

      parse(line, whole(_)) match {
        case Parsed.Success(value, _) => value
        case f: Parsed.Failure        => sys.error(f.toString())
      }
    }
  }

  private final case class Point(x: Int, y: Int)
  private final case class Board(
      spots: Map[Point, RealSpot],
      spotsAtRow: Map[Int, Chunk[(Point, RealSpot)]],
      spotsAtCol: Map[Int, Chunk[(Point, RealSpot)]],
  ) {
    def addSpot(point: Point, spot: RealSpot): Board = {
      val newSpots = spots + (point -> spot)
      val newAtRow = {
        val old     = spotsAtRow.getOrElse(point.y, Chunk.empty)
        val updated = (old :+ (point -> spot))
        spotsAtRow + (point.y -> updated)
      }
      val newAtCol = {
        val old     = spotsAtCol.getOrElse(point.x, Chunk.empty)
        val updated = (old :+ (point -> spot))
        spotsAtCol + (point.x -> updated)
      }
      copy(spots = newSpots, spotsAtRow = newAtRow, spotsAtCol = newAtCol)
    }

    def tryToMove(point: Point, inDirection: Facing): Point =
      inDirection match {
        case Facing.Left | Facing.Right =>
          val row                     = spotsAtRow(point.y)
          val (newPoint, spotAtPoint) = loopNeighbor(row, point.x - row.head._1.x, inDirection == Facing.Right)
          if (spotAtPoint == Spot.Wall) point else newPoint

        case _ =>
          val col                     = spotsAtCol(point.x)
          val (newPoint, spotAtPoint) = loopNeighbor(col, point.y - col.head._1.y, inDirection == Facing.Down)
          if (spotAtPoint == Spot.Wall) point else newPoint
      }

    @tailrec
    def moveCount(point: Point, inDirection: Facing, numMoves: Int): Point =
      if (numMoves <= 0) point
      else moveCount(tryToMove(point, inDirection), inDirection, numMoves - 1)

    def leftmostPointAtRow(row: Int): Point = spotsAtRow(row).head._1
  }

  private sealed trait ParseStage
  private object ParseStage {
    case object Board  extends ParseStage
    case object Instrs extends ParseStage
    case object Done   extends ParseStage
  }
  private final case class ParseState(
      currBoard: Board = Board(Map.empty, Map.empty, Map.empty),
      instrs: Chunk[Instr] = Chunk.empty,
      stage: ParseStage = ParseStage.Board,
  )
  private def parseFile(dataFile: String) = resourceLines(dataFile).zipWithIndex.runFold(ParseState()) {
    case (acc, (line, _)) if line.isEmpty && acc.stage == ParseStage.Board => acc.copy(stage = ParseStage.Instrs)

    case (acc, (line, _)) if acc.stage == ParseStage.Instrs =>
      acc.copy(instrs = Instr.parseInstrs(line), stage = ParseStage.Done)

    case (acc, (line, idx)) if acc.stage == ParseStage.Board =>
      acc.copy(currBoard = line.zipWithIndex.foldLeft(acc.currBoard) { case (board, (c, col)) =>
        Spot.charMap(c) match {
          case s: RealSpot => board.addSpot(Point(col + 1, idx.toInt + 1), s)
          case _           => board
        }
      })

    case (acc, _) => acc
  }

  override def part1TestExpectation: Any = 6_032
  override def part1Expectation: Any     = 126_350

  override def part1(dataFile: String): STask[Any] = parseFile(dataFile).map { case ParseState(board, instrs, _) =>
    val startingPoint          = board.leftmostPointAtRow(1)
    val startingFacing: Facing = Facing.Right

    val (endingPoint, endingFacing) = instrs.foldLeft(startingPoint -> startingFacing) {
      case ((currPoint, currFacing), Instr.Turn(t)) => currPoint                             -> currFacing.turn(t)
      case ((currPoint, facing), Instr.Move(n))     => board.moveCount(currPoint, facing, n) -> facing
    }

    endingPoint.y * 1000 + endingPoint.x * 4 + endingFacing.ord
  }
}
