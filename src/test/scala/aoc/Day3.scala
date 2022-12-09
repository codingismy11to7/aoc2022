package aoc

import zio._
import zio.stream._

object Day3 extends ZIOAppDefault {
  final val Test  = false
  final val Part1 = false

  final case class Line(line: String) {
    lazy val charSet: Set[Char] = line.toSet
    lazy val itemSharedBetweenCompartments: Char = {
      val (comp1, comp2) = line.splitAt(line.length / 2)
      (comp1.toSet & comp2.toSet).head
    }
  }

  private def addItemPriorities[E](items: Stream[E, Char]) =
    items.map(item => if (item > 'Z') 1 + (item - 'a') else 27 + (item - 'A')).runSum

  private val lines = resourceLines(s"3/${if (Test) "test" else "input"}.txt").map(Line)

  private val part1 = addItemPriorities {
    lines.map(_.itemSharedBetweenCompartments)
  }

  private val part2 = addItemPriorities {
    lines.map(_.charSet).grouped(3).map(_.reduce(_ & _).head)
  }

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (if (Part1) part1 else part2).flatMap(ZIO.debug(_))
}
