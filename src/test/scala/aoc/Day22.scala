package aoc

import com.softwaremill.quicklens._
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

    def modify(point: Point): Point = this match {
      case Facing.Left  => point.modify(_.x)(_ - 1)
      case Facing.Right => point.modify(_.x)(_ + 1)
      case Facing.Down  => point.modify(_.y)(_ + 1)
      case Facing.Up    => point.modify(_.y)(_ - 1)
    }
  }
  private object Facing extends Enum[Facing] {
    override def values: IndexedSeq[Facing] = findValues
    case object Right extends Facing
    case object Down  extends Facing
    case object Left  extends Facing
    case object Up    extends Facing
  }

  private sealed trait CubeFace
  private object CubeFace {
    case object Front  extends CubeFace
    case object Back   extends CubeFace
    case object Left   extends CubeFace
    case object Right  extends CubeFace
    case object Top    extends CubeFace
    case object Bottom extends CubeFace
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
  private final case class Delta(dX: Int, dY: Int)
  private def Pt(x: Int, y: Int): Point = Point(x, y)
  private final case class FaceBounds(tl: Point, br: Point) {
    def mirrored(delt: Delta): Point = Point(br.x - delt.dX, br.y - delt.dY)

    def contains(p: Point): Boolean =
      p.x >= tl.x &&
        p.y >= tl.y &&
        p.x <= br.x &&
        p.y <= br.y

    def getDelta(of: Point): Delta = Delta(of.x - tl.x, of.y - tl.y)
    def toReal(of: Delta): Point   = Point(tl.x + of.dX, tl.y + of.dY)
  }
  private sealed trait CoordMod
  private object CoordMod {
    case object MirrorOfX extends CoordMod
    case object MirrorOfY extends CoordMod
    case object SameAsX   extends CoordMod
    case object SameAsY   extends CoordMod
  }
  private final case class ExitTransitionRule(
      direction: Facing,
      arrivesAt: CubeFace,
      arrivesFacing: Facing,
      mod: CoordMod,
  )
  private final case class FaceData(bounds: FaceBounds, transitionRules: Seq[ExitTransitionRule])
  private final case class Board(
      cubeFaceData: Map[CubeFace, FaceData],
      spots: Map[Point, RealSpot],
      spotsAtRow: Map[Int, Chunk[(Point, RealSpot)]],
      spotsAtCol: Map[Int, Chunk[(Point, RealSpot)]],
  ) {
    def addSpot(point: Point, spot: RealSpot): Board = {
      val newSpots = spots + (point -> spot)
      val newAtRow = {
        val old     = spotsAtRow.getOrElse(point.y, Chunk.empty)
        val updated = old :+ (point -> spot)
        spotsAtRow + (point.y -> updated)
      }
      val newAtCol = {
        val old     = spotsAtCol.getOrElse(point.x, Chunk.empty)
        val updated = old :+ (point -> spot)
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

    def tryToMoveCube(point: Point, inDirection: Facing): (Point, Facing) = {
      val tryPoint = inDirection.modify(point)
      if (spots.contains(tryPoint)) {
        (spots(tryPoint) match {
          case Spot.Wall  => point
          case Spot.Empty => tryPoint
        }) -> inDirection
      } else {
        val data              = cubeFaceData.values.find(_.bounds.contains(point)).get
        val rule              = data.transitionRules.find(_.direction == inDirection).get
        val newCubeFace       = rule.arrivesAt
        val newCubeFaceBounds = cubeFaceData(newCubeFace).bounds
        val newFacing         = rule.arrivesFacing

        val delt = data.bounds.getDelta(point)
        val newX = newFacing match {
          case Facing.Right => newCubeFaceBounds.tl.x
          case Facing.Left  => newCubeFaceBounds.br.x
          case _ =>
            (rule.mod match {
              case CoordMod.SameAsY   => newCubeFaceBounds.toReal(Delta(delt.dY, 0))
              case CoordMod.SameAsX   => newCubeFaceBounds.toReal(delt)
              case CoordMod.MirrorOfX => newCubeFaceBounds.mirrored(delt)
              case CoordMod.MirrorOfY => newCubeFaceBounds.mirrored(Delta(delt.dY, 0))
            }).x
        }
        val newY = newFacing match {
          case Facing.Down => newCubeFaceBounds.tl.y
          case Facing.Up   => newCubeFaceBounds.br.y
          case _ =>
            (rule.mod match {
              case CoordMod.SameAsY   => newCubeFaceBounds.toReal(delt)
              case CoordMod.SameAsX   => newCubeFaceBounds.toReal(Delta(0, delt.dX))
              case CoordMod.MirrorOfX => newCubeFaceBounds.mirrored(Delta(0, delt.dX))
              case CoordMod.MirrorOfY => newCubeFaceBounds.mirrored(delt)
            }).y
        }

        val newPoint = Point(newX, newY)
        spots(newPoint) match {
          case Spot.Wall  => point    -> inDirection
          case Spot.Empty => newPoint -> newFacing
        }
      }
    }

    @tailrec
    def moveCount(point: Point, inDirection: Facing, numMoves: Int): (Point, Facing) =
      if (numMoves <= 0) point -> inDirection
      else moveCount(tryToMove(point, inDirection), inDirection, numMoves - 1)

    @tailrec
    def moveCubeCount(point: Point, inDirection: Facing, numMoves: Int): (Point, Facing) =
      if (numMoves <= 0) point -> inDirection
      else {
        val (newPoint, newDir) = tryToMoveCube(point, inDirection)
        moveCubeCount(newPoint, newDir, numMoves - 1)
      }

    def leftmostPointAtRow(row: Int): Point = spotsAtRow(row).head._1
  }

  private sealed trait ParseStage
  private object ParseStage {
    case object Board  extends ParseStage
    case object Instrs extends ParseStage
    case object Done   extends ParseStage
  }
  private final case class ParseState(currBoard: Board, instrs: Chunk[Instr], stage: ParseStage)
  private object ParseState {
    def empty(cubeBounds: Map[CubeFace, FaceData]): ParseState =
      ParseState(Board(cubeBounds, Map.empty, Map.empty, Map.empty), Chunk.empty, ParseStage.Board)
  }
  private def parseFile(dataFile: String) = {
    val cubeBounds: Map[CubeFace, FaceData] =
      if (dataFile.contains("test"))
        Map(
          CubeFace.Top -> FaceData(
            FaceBounds(Pt(9, 1), Pt(12, 4)),
            Seq(
              ExitTransitionRule(Facing.Right, CubeFace.Right, Facing.Left, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Left, CubeFace.Left, Facing.Down, CoordMod.SameAsY),
              ExitTransitionRule(Facing.Up, CubeFace.Back, Facing.Down, CoordMod.MirrorOfX),
            ),
          ),
          CubeFace.Back -> FaceData(
            FaceBounds(Pt(1, 5), Pt(4, 8)),
            Seq(
              ExitTransitionRule(Facing.Left, CubeFace.Right, Facing.Up, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Up, CubeFace.Top, Facing.Down, CoordMod.MirrorOfX),
              ExitTransitionRule(Facing.Down, CubeFace.Bottom, Facing.Up, CoordMod.MirrorOfX),
            ),
          ),
          CubeFace.Left -> FaceData(
            FaceBounds(Pt(5, 5), Pt(8, 8)),
            Seq(
              ExitTransitionRule(Facing.Up, CubeFace.Top, Facing.Right, CoordMod.SameAsX),
              ExitTransitionRule(Facing.Down, CubeFace.Bottom, Facing.Right, CoordMod.MirrorOfX),
            ),
          ),
          CubeFace.Front -> FaceData(
            FaceBounds(Pt(9, 5), Pt(12, 8)),
            Seq(ExitTransitionRule(Facing.Right, CubeFace.Right, Facing.Down, CoordMod.MirrorOfY)),
          ),
          CubeFace.Bottom -> FaceData(
            FaceBounds(Pt(9, 9), Pt(12, 12)),
            Seq(
              ExitTransitionRule(Facing.Left, CubeFace.Left, Facing.Up, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Down, CubeFace.Back, Facing.Up, CoordMod.MirrorOfX),
            ),
          ),
          CubeFace.Right -> FaceData(
            FaceBounds(Pt(13, 9), Pt(16, 12)),
            Seq(
              ExitTransitionRule(Facing.Up, CubeFace.Front, Facing.Left, CoordMod.MirrorOfX),
              ExitTransitionRule(Facing.Right, CubeFace.Top, Facing.Left, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Down, CubeFace.Back, Facing.Right, CoordMod.MirrorOfX),
            ),
          ),
        )
      else
        Map(
          CubeFace.Top -> FaceData(
            FaceBounds(Pt(51, 1), Pt(100, 50)),
            Seq(
              ExitTransitionRule(Facing.Left, CubeFace.Left, Facing.Right, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Up, CubeFace.Back, Facing.Right, CoordMod.SameAsX),
            ),
          ),
          CubeFace.Right -> FaceData(
            FaceBounds(Pt(101, 1), Pt(150, 50)),
            Seq(
              ExitTransitionRule(Facing.Up, CubeFace.Back, Facing.Up, CoordMod.SameAsX),
              ExitTransitionRule(Facing.Right, CubeFace.Bottom, Facing.Left, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Down, CubeFace.Front, Facing.Left, CoordMod.SameAsX),
            ),
          ),
          CubeFace.Front -> FaceData(
            FaceBounds(Pt(51, 51), Pt(100, 100)),
            Seq(
              ExitTransitionRule(Facing.Left, CubeFace.Left, Facing.Down, CoordMod.SameAsY),
              ExitTransitionRule(Facing.Right, CubeFace.Right, Facing.Up, CoordMod.SameAsY),
            ),
          ),
          CubeFace.Left -> FaceData(
            FaceBounds(Pt(1, 101), Pt(50, 150)),
            Seq(
              ExitTransitionRule(Facing.Left, CubeFace.Top, Facing.Right, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Up, CubeFace.Front, Facing.Right, CoordMod.SameAsX),
            ),
          ),
          CubeFace.Bottom -> FaceData(
            FaceBounds(Pt(51, 101), Pt(100, 150)),
            Seq(
              ExitTransitionRule(Facing.Right, CubeFace.Right, Facing.Left, CoordMod.MirrorOfY),
              ExitTransitionRule(Facing.Down, CubeFace.Back, Facing.Left, CoordMod.SameAsX),
            ),
          ),
          CubeFace.Back -> FaceData(
            FaceBounds(Pt(1, 151), Pt(50, 200)),
            Seq(
              ExitTransitionRule(Facing.Left, CubeFace.Top, Facing.Down, CoordMod.SameAsY),
              ExitTransitionRule(Facing.Down, CubeFace.Right, Facing.Down, CoordMod.SameAsX),
              ExitTransitionRule(Facing.Right, CubeFace.Bottom, Facing.Up, CoordMod.SameAsY),
            ),
          ),
        )

    resourceLines(dataFile).zipWithIndex.runFold(ParseState.empty(cubeBounds)) {
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
  }

  private def app(move: (Board, Point, Facing, Int) => (Point, Facing))(dataFile: String) = parseFile(dataFile).map {
    case ParseState(board, instrs, _) =>
      val startingPoint          = board.leftmostPointAtRow(1)
      val startingFacing: Facing = Facing.Right

      val (endingPoint, endingFacing) = instrs.foldLeft(startingPoint -> startingFacing) {
        case ((currPoint, currFacing), Instr.Turn(t)) => currPoint -> currFacing.turn(t)
        case ((currPoint, facing), Instr.Move(n))     => move(board, currPoint, facing, n)
      }

      endingPoint.y * 1000 + endingPoint.x * 4 + endingFacing.ord
  }

  override def part1TestExpectation: Any           = 6_032
  override def part1Expectation: Any               = 126_350
  override def part1(dataFile: String): STask[Any] = app(_.moveCount(_, _, _))(dataFile)

  override def part2TestExpectation: Any           = 5_031
  override def part2Expectation: Any               = 129_339
  override def part2(dataFile: String): STask[Any] = app(_.moveCubeCount(_, _, _))(dataFile)
}
