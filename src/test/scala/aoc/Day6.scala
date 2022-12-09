package aoc

import zio._

object Day6 extends AdventDay {
  override final val day          = 6
  override val numberOfTests: Int = 5

  private def app(dataFile: String)(numDistinct: Int) =
    resourceChars(dataFile).zipWithIndex
      .sliding(numDistinct)
      .find(_.map(_._1).toSet.size == numDistinct)
      .mapConcat(_.lastOption.map(_._2 + 1))
      .runHead
      .map(_.get)

  override def part1TestExpectation: Any           = Chunk(7, 5, 6, 10, 11)
  override def part1Expectation: Any               = 1_140
  override def part1(dataFile: String): STask[Any] = app(dataFile)(4)

  override def part2TestExpectation: Any           = Chunk(19, 23, 23, 29, 26)
  override def part2Expectation: Any               = 3_495
  override def part2(dataFile: String): STask[Any] = app(dataFile)(14)
}
