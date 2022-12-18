package aoc

import com.softwaremill.quicklens._
import zio._
import zio.prelude.fx.ZPure

import scala.annotation.tailrec

object Day18 extends AdventDay {
  override final val day = 18

  private final case class Coords(x: Int, y: Int, z: Int) { self =>
    def adjacent: Chunk[Coords] = Chunk(
      self.modify(_.x)(_ + 1),
      self.modify(_.x)(_ - 1),
      self.modify(_.y)(_ + 1),
      self.modify(_.y)(_ - 1),
      self.modify(_.z)(_ + 1),
      self.modify(_.z)(_ - 1),
    )

    def min(other: Coords): Coords =
      Coords(math.min(self.x, other.x), math.min(self.y, other.y), math.min(self.z, other.z))
    def max(other: Coords): Coords =
      Coords(math.max(self.x, other.x), math.max(self.y, other.y), math.max(self.z, other.z))
    def +(i: Int): Coords = Coords(x + i, y + i, z + i)
    def -(i: Int): Coords = self + (0 - i)

    def within(min: Coords, max: Coords): Boolean =
      self.x >= min.x && self.y >= min.y && self.z >= min.z &&
        self.x <= max.x && self.y <= max.y && self.z <= max.z
  }
  private object Coords {
    def apply(s: String): Coords = ZPure.succeed(s.split(',')).map(a => Coords(a(0).toInt, a(1).toInt, a(2).toInt)).run
  }

  private def coords(dataFile: String) = resourceLines(dataFile).filterNot(_.isEmpty).map(Coords(_))

  override def part1TestExpectation: Any = 64
  override def part1Expectation: Any     = 4_300

  override def part1(dataFile: String): STask[Any] = coords(dataFile).runCollect.map { coords =>
    val set = coords.toSet
    coords.foldLeft(0)((acc, c) => acc + c.adjacent.filterNot(set.contains).size)
  }

  private final case class State(
      coords: Chunk[Coords] = Chunk.empty,
      mins: Coords = Coords(Int.MaxValue, Int.MaxValue, Int.MaxValue),
      maxes: Coords = Coords(Int.MinValue, Int.MinValue, Int.MinValue),
  ) {
    def append(c: Coords): State = State(coords :+ c, mins min c, maxes max c)
  }

  override def part2TestExpectation: Any = 58
  override def part2Expectation: Any     = 2_490

  override def part2(dataFile: String): STask[Any] = coords(dataFile).runFold(State())(_ append _).map { state =>
    val droplet = state.coords.toSet
    val min     = state.mins - 1
    val max     = state.maxes + 1

    @tailrec
    def loop(toVisit: Set[Coords], visited: Set[Coords], acc: Int): Int =
      toVisit.headOption match {
        case None => acc
        case Some(curr) =>
          val rest                          = toVisit - curr
          val newVisited                    = visited + curr
          val unvisitedAdjacentWithinBounds = curr.adjacent.filter(_.within(min, max)).filterNot(visited.contains)
          val (dropCoords, airCoords)       = unvisitedAdjacentWithinBounds.partition(droplet.contains)
          loop(rest ++ airCoords, newVisited, acc + dropCoords.size)
      }

    loop(Set(min), Set.empty, 0)
  }
}
