package aoc

import zio._

import scala.annotation.tailrec

object Day14 extends AdventDay {
  override final val day = 14

  private sealed trait Spot
  private object Spot {
    case object Air  extends Spot
    case object Rock extends Spot
    case object Sand extends Spot
  }

  private implicit class RichChunk[A](private val c: Chunk[A]) extends AnyVal {
    def updateAt(idx: Int)(f: A => A): Chunk[A] = c.updated(idx, f(c(idx)))
  }

  private final case class Cave(spots: Chunk[Chunk[Spot]], minX: Int, maxYPos: Int) {
    private lazy val maxXPos = minX + spots.head.size - 1
    private def expanded = {
      val growBy = spots.head.size
      val empty  = List.fill(growBy)(Spot.Air)
      copy(spots.map(x => Chunk.fromIterable(empty ++ x ++ empty)), minX - growBy)
    }

    /*
    private def rendered = spots
      .map(l => l.map { case Spot.Air => '.'; case Spot.Rock => '#'; case Spot.Sand => 'o' }.mkString)
      .mkString("\n")
     */

    def spotAt(coord: Coord): Spot              = spots(coord.y)(coord.x - minX)
    private def setAt(coord: Coord, spot: Spot) = copy(spots.updateAt(coord.y)(_.updated(coord.x - minX, spot)))
    def putSandAt(coord: Coord): Cave           = setAt(coord, Spot.Sand)
    def putRockAt(coord: Coord): Cave           = setAt(coord, Spot.Rock)
    def dropInSand: Option[Cave] = {
      @tailrec
      def loop(curr: Coord = Coord(500, 0)): Option[Cave] =
        if (curr.down.y == maxYPos && curr.nextSpots.map(spotAt).contains(Spot.Air)) None
        else
          curr.nextSpots.find(spotAt(_) == Spot.Air) match {
            case Some(c) => loop(c)
            case None    => Some(putSandAt(curr))
          }
      loop()
    }

    def dropInSandInfiniteFloor: Option[Cave] = {
      @tailrec
      def loop(curr: Coord = Coord(500, 0)): Option[Cave] =
        if (spotAt(Coord(500, 0)) == Spot.Sand) None
        else if (curr.x == minX || curr.x == maxXPos) expanded.dropInSandInfiniteFloor
        else if (curr.y == maxYPos + 1) Some(putSandAt(curr))
        else
          curr.nextSpots.find(spotAt(_) == Spot.Air) match {
            case Some(c) => loop(c)
            case None    => Some(putSandAt(curr))
          }
      loop()
    }
  }
  private object Cave {
    def create(minX: Int, maxX: Int, maxY: Int): Cave =
      Cave(Chunk.fill(maxY + 2)(Chunk.fill(1 + (maxX - minX))(Spot.Air)), minX, maxY)
  }

  private final case class Coord(x: Int, y: Int) {
    lazy val down: Coord           = copy(y = 1 + y)
    lazy val downLeft: Coord       = down.copy(x = x - 1)
    lazy val downRight: Coord      = down.copy(x = x + 1)
    lazy val nextSpots: Seq[Coord] = List(down, downLeft, downRight)
  }
  private object Coord {
    private val re = """(\d+),(\d+)""".r
    def parse(s: String): Coord = s match {
      case re(x, y) => Coord(x.toInt, y.toInt)
      case _        => sys.error(s"invalid coord $s")
    }
  }

  private def coordLine(a: Coord, b: Coord) = {
    def range(a: Int, b: Int) = math.min(a, b) to math.max(a, b)
    if (a.x == b.x) range(a.y, b.y).map(Coord(a.x, _))
    else range(a.x, b.x).map(Coord(_, a.y))
  }

  private def parseLine(line: String) = Chunk.fromArray(line.split("""\s*->\s*""")).map(Coord.parse)

  private def createCave(dataFile: String) = resourceLines(dataFile)
    .map(parseLine)
    .runFold((Chunk.empty[Chunk[Coord]], Int.MaxValue, Int.MinValue, Int.MinValue)) {
      case ((accLines, accMinX, accMaxX, accMaxY), thisLine) =>
        val newLines = accLines :+ thisLine
        val newMinX  = (accMinX +: thisLine.map(_.x)).min
        val newMaxX  = (accMaxX +: thisLine.map(_.x)).max
        val newMaxY  = (accMaxY +: thisLine.map(_.y)).max
        (newLines, newMinX, newMaxX, newMaxY)
    }
    .map { case (allLines, minX, maxX, maxY) =>
      def addLineToCave(cave: Cave, line: Chunk[Coord]): Cave = line.sliding(2).foldLeft(cave) { (oldCave, coordPair) =>
        coordLine(coordPair(0), coordPair(1)).foldLeft(oldCave)(_ putRockAt _)
      }

      allLines.foldLeft(Cave.create(minX - 1, maxX + 1, maxY))(addLineToCave)
    }

  private def app(dataFile: String)(sandDropper: Cave => Option[Cave]) =
    createCave(dataFile).map { cave =>
      @tailrec
      def loop(currCave: Cave = cave, currSandCount: Int = 0): Int =
        sandDropper(currCave) match {
          case None          => currSandCount
          case Some(newCave) => loop(newCave, 1 + currSandCount)
        }

      loop()
    }

  override def part1TestExpectation: Any           = 24
  override def part1Expectation: Any               = 793
  override def part1(dataFile: String): STask[Any] = app(dataFile)(_.dropInSand)

  override def part2TestExpectation: Any           = 93
  override def part2Expectation: Any               = 24_166
  override def part2(dataFile: String): STask[Any] = app(dataFile)(_.dropInSandInfiniteFloor)
}
