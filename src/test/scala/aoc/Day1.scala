package aoc

import zio._

object Day1 extends ZIOAppDefault {
  private val data = resourceLines("1/input.txt")

  private val highest = data
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

  private val topThree = data
    .runFold(Chunk[Int]() -> Chunk[Int]()) { case ((accElves, accCurrElf), thisLine) =>
      if (thisLine.isEmpty) {
        (accElves :+ accCurrElf.sum) -> Chunk()
      } else {
        val thisValue = thisLine.toInt
        accElves -> (accCurrElf :+ thisValue)
      }
    }
    .map(_._1.sorted.reverse.take(3).sum)

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (if (false) highest else topThree).flatMap(ZIO.debug(_))
}
