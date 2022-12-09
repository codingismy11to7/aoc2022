package aoc

import com.softwaremill.quicklens._
import enumeratum._
import zio._

import scala.annotation.tailrec

object Day8 extends AdventDay {
  override final val day = 8

  private final case class Position(x: Int, y: Int)

  private sealed trait Highest { def height: Int }
  private final case class ItsMe(height: Int)    extends Highest
  private final case class ImHidden(height: Int) extends Highest

  private sealed trait Direction extends EnumEntry {
    def spotIsFurthest(spot: MatrixSpot, matrix: Matrix): Boolean
    def decrementPos(spot: MatrixSpot, matrix: Matrix): Position
  }
  private object Direction extends Enum[Direction] {
    val values: IndexedSeq[Direction] = findValues
    case object Left extends Direction {
      override def spotIsFurthest(spot: MatrixSpot, matrix: Matrix): Boolean = spot.pos.x == 0
      override def decrementPos(spot: MatrixSpot, matrix: Matrix): Position  = spot.pos.modify(_.x)(_ - 1)
    }
    case object Right extends Direction {
      override def spotIsFurthest(spot: MatrixSpot, matrix: Matrix): Boolean = spot.pos.x == (matrix.cols - 1)
      override def decrementPos(spot: MatrixSpot, matrix: Matrix): Position  = spot.pos.modify(_.x)(_ + 1)
    }
    case object Top extends Direction {
      override def spotIsFurthest(spot: MatrixSpot, matrix: Matrix): Boolean = spot.pos.y == 0
      override def decrementPos(spot: MatrixSpot, matrix: Matrix): Position  = spot.pos.modify(_.y)(_ - 1)
    }
    case object Bottom extends Direction {
      override def spotIsFurthest(spot: MatrixSpot, matrix: Matrix): Boolean = spot.pos.y == (matrix.rows - 1)
      override def decrementPos(spot: MatrixSpot, matrix: Matrix): Position  = spot.pos.modify(_.y)(_ + 1)
    }
  }

  private final case class Matrix(m: Chunk[Chunk[MatrixSpot]]) {
    def spotAt(pos: Position): MatrixSpot = m(pos.y)(pos.x)
    val rows: Int                         = m.size
    val cols: Int                         = m.head.size
  }

  private final case class MatrixSpot(height: Int, pos: Position) { self =>
    private var highest = Map.empty[Direction, Highest]
    def getHighestInDirection(matrix: Matrix)(direction: Direction): Highest =
      highest.get(direction) match {
        case Some(h) => h

        case None =>
          val a =
            if (direction.spotIsFurthest(self, matrix)) ItsMe(height)
            else {
              val otherA = matrix.spotAt(direction.decrementPos(self, matrix)).getHighestInDirection(matrix)(direction)
              if (otherA.height < height) ItsMe(height) else ImHidden(otherA.height)
            }

          highest += (direction -> a)
          a
      }

    def visibleFromDirection(matrix: Matrix)(direction: Direction): Boolean =
      getHighestInDirection(matrix)(direction) match {
        case ItsMe(_) => true
        case _        => false
      }

    def visible(matrix: Matrix): Boolean = Direction.values.exists(visibleFromDirection(matrix))

    def scenicScore(matrix: Matrix): Int =
      if (Direction.values.exists(_.spotIsFurthest(self, matrix))) 0
      else {
        def scoreFor(d: Direction): Int = {
          def getNext(spot: MatrixSpot) = matrix.spotAt(d.decrementPos(spot, matrix))
          // already pre-checked and know it's not the furthest in any direction so this is safe
          val nextSpot = getNext(self)
          @tailrec
          def loop(currSpot: MatrixSpot = nextSpot, acc: Int = 1): Int =
            if (d.spotIsFurthest(currSpot, matrix) || currSpot.height >= self.height) acc
            else loop(getNext(currSpot), 1 + acc)
          loop()
        }

        Direction.values.foldLeft(1) {
          case (0, _)   => 0
          case (acc, d) => acc * scoreFor(d)
        }
      }
  }

  private def matrix(dataFile: String) = resourceLines(dataFile).runCollect
    .map(_.zipWithIndex.map { case (s, yIdx) =>
      Chunk.fromArray(s.toCharArray).map(_ - '0').zipWithIndex.map { case (h, xIdx) =>
        MatrixSpot(h, Position(xIdx, yIdx))
      }
    })
    .map(Matrix)

  private def allSpots(matrix: Matrix) = matrix.m.flatMap(identity)

  override def part1TestExpectation: Any = 21
  override def part1Expectation: Any     = 1_785

  override def part1(dataFile: String): STask[Any] = matrix(dataFile).map { matrix =>
    allSpots(matrix).count(_.visible(matrix))
  }

  override def part2TestExpectation: Any = 8
  override def part2Expectation: Any     = 345_168

  override def part2(dataFile: String): STask[Any] = matrix(dataFile).map { matrix =>
    allSpots(matrix).map(_.scenicScore(matrix)).max
  }
}
