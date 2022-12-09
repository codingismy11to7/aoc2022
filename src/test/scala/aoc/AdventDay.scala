package aoc

import aoc.AdventDay.NoAnswer
import zio._

object AdventDay {
  case object NoAnswer
}

trait AdventDay {
  def day: Int

  def numberOfTests = 1
  def testFiles: Chunk[String] =
    if (numberOfTests == 1) Chunk(s"$day/test.txt") else Chunk.from(1 to numberOfTests).map(i => s"$day/test$i.txt")

  def inputFile = s"$day/input.txt"

  def part1(dataFile: String): STask[Any] = ZIO.succeed(NoAnswer)
  def part1Expectation: Any               = NoAnswer
  def part1TestExpectation: Any           = NoAnswer

  def part2(dataFile: String): STask[Any] = ZIO.succeed(NoAnswer)
  def part2Expectation: Any               = NoAnswer
  def part2TestExpectation: Any           = NoAnswer
}
