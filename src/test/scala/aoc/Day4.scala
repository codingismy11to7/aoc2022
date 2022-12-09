package aoc

import zio._

object Day4 extends ZIOAppDefault {
  final val Test  = false
  final val Part1 = false

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

  private val data        = resourceLines(s"4/${if (Test) "test" else "input"}.txt")
  private val assignments = data.mapZIO(SectionAssignment.create)

  private def app(f: SectionAssignment => Boolean) = assignments.filter(f).runCount

  private lazy val part1 = app(_.oneRangeContainsTheOther)
  private lazy val part2 = app(_.rangesOverlap)

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (if (Part1) part1 else part2).flatMap(ZIO.debug(_))
}
