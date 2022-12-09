package aoc

import zio.stream._

object Day3 extends AdventDay {
  override final val day = 3

  final case class Line(line: String) {
    lazy val charSet: Set[Char] = line.toSet
    lazy val itemSharedBetweenCompartments: Char = {
      val (comp1, comp2) = line.splitAt(line.length / 2)
      (comp1.toSet & comp2.toSet).head
    }
  }

  private def addItemPriorities[E](items: Stream[E, Char]) =
    items.map(item => if (item > 'Z') 1 + (item - 'a') else 27 + (item - 'A')).runSum

  private def lines(dataFile: String) = resourceLines(dataFile).map(Line)

  override def part1TestExpectation: Any = 157
  override def part1Expectation: Any     = 8_298

  override def part1(dataFile: String): STask[Any] = addItemPriorities {
    lines(dataFile).map(_.itemSharedBetweenCompartments)
  }

  override def part2TestExpectation: Any = 70
  override def part2Expectation: Any     = 2_708

  override def part2(dataFile: String): STask[Any] = addItemPriorities {
    lines(dataFile).map(_.charSet).grouped(3).map(_.reduce(_ & _).head)
  }
}
