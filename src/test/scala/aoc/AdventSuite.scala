package aoc

import aoc.AdventDay.NoAnswer
import zio._
import zio.test._

object AdventSuite extends ZIOSpecDefault {
  final val Days = Chunk(Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12)

  private def createTests(day: AdventDay) = {
    def doTest(part1: Boolean, dataFile: String, expected: Any) = for {
      ans <- if (part1) day.part1(dataFile) else day.part2(dataFile)
      _   <- ZIO.debug(s"Answer: $ans").when(ans != NoAnswer && expected == NoAnswer)
    } yield {
      if (expected != NoAnswer) assertTrue(ans == expected)
      else assertCompletes
    }

    def makeTestDataTests(part1: Boolean) = day.testFiles.zipWithIndex.map { case (dataFile, idx) =>
      val expected = (if (part1) day.part1TestExpectation else day.part2TestExpectation) match {
        case NoAnswer                   => NoAnswer
        case c: Chunk[_]                => c(idx)
        case _ if day.numberOfTests > 1 => sys.error("Multiple tests should have multiple expectations")
        case value                      => value
      }

      test(s"Test Input${if (day.numberOfTests == 1) "" else " " + (1 + idx)}") {
        doTest(part1, dataFile, expected)
      }
    }

    def makeInputDataTest(part1: Boolean) = test("Your puzzle input") {
      doTest(part1, day.inputFile, if (part1) day.part1Expectation else day.part2Expectation)
    }

    Chunk(
      suite("Part 1")(makeTestDataTests(true) :+ makeInputDataTest(true): _*),
      suite("Part 2")(makeTestDataTests(false) :+ makeInputDataTest(false): _*),
    )
  }

  private def createDay(day: AdventDay) =
    suite(s"Day ${day.day}")(
      createTests(day): _*,
    )

  private def createDays = Days.map(createDay)

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Advent of Code")(createDays: _*) @@ TestAspect.timed @@ TestAspect.sequential
}
