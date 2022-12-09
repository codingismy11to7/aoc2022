package aoc

import zio._

object Day1 extends AdventDay {
  override final val day = 1

  private def data(dataFile: String) = resourceLines(dataFile)

  override def part1TestExpectation: Any = 24_000
  override def part1Expectation: Any     = 66_186

  override def part1(dataFile: String): STask[Int] = data(dataFile)
    .runFold(Chunk[Int]() -> Int.MinValue) { case ((accCurrItems, currHighest), thisLine) =>
      if (thisLine.isEmpty) {
        val sum = accCurrItems.sum
        Chunk() -> math.max(currHighest, sum)
      } else {
        val thisValue = thisLine.toInt
        (accCurrItems :+ thisValue) -> currHighest
      }
    }
    .map(_._2)

  override def part2TestExpectation: Any = 45_000
  override def part2Expectation: Any     = 196_804

  override def part2(dataFile: String): STask[Int] = data(dataFile)
    .runFold(Chunk[Int]() -> Chunk[Int]()) { case ((accElves, accCurrElf), thisLine) =>
      if (thisLine.isEmpty) {
        (accElves :+ accCurrElf.sum) -> Chunk()
      } else {
        val thisValue = thisLine.toInt
        accElves -> (accCurrElf :+ thisValue)
      }
    }
    .map { case (accElves, accCurrElf) => if (accCurrElf.nonEmpty) accElves :+ accCurrElf.sum else accElves }
    .map(_.sorted.reverse.take(3).sum)

}
