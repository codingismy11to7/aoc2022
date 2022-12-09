package aoc

import zio._

object Day4 extends AdventDay {
  override final val day = 4

  implicit class RichRange(private val v: Range) extends AnyVal {
    def isContainedIn(o: Range): Boolean = o.contains(v.start) && o.contains(v.end)
    def overlaps(o: Range): Boolean      = o.contains(v.start) || o.contains(v.end)
  }

  final case class SectionAssignment(elfA: Range, elfB: Range) {
    def oneRangeContainsTheOther: Boolean = elfA.isContainedIn(elfB) || elfB.isContainedIn(elfA)
    def rangesOverlap: Boolean            = elfA.overlaps(elfB) || elfB.overlaps(elfA)
  }
  object SectionAssignment {
    private val lineRe                         = """(\d+)-(\d+),(\d+)-(\d+)""".r
    private def incRange(s: String, e: String) = s.toInt to e.toInt
    val create: String => STask[SectionAssignment] = {
      case lineRe(aBeg, aEnd, bBeg, bEnd) => attempt(SectionAssignment(incRange(aBeg, aEnd), incRange(bBeg, bEnd)))
      case line                           => ZIO.fail(s"Invalid line $line")
    }
  }

  private def app(dataFile: String)(f: SectionAssignment => Boolean) = {
    val data        = resourceLines(dataFile)
    val assignments = data.mapZIO(SectionAssignment.create)

    assignments.filter(f).runCount
  }

  override def part1TestExpectation: Any           = 2
  override def part1Expectation: Any               = 542
  override def part1(dataFile: String): STask[Any] = app(dataFile)(_.oneRangeContainsTheOther)

  override def part2TestExpectation: Any           = 4
  override def part2Expectation: Any               = 900
  override def part2(dataFile: String): STask[Any] = app(dataFile)(_.rangesOverlap)
}
