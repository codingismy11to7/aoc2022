package aoc

import zio._
import zio.stream._

object Day6 extends ZIOAppDefault {
  final val Test  = false
  final val Part1 = false

  private val testInputs = Chunk(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
  )

  private val data = resourceChars("6/input.txt")

  private def app(numDistinct: Int) = {
    def getSolution[E](s: Stream[E, Char]) =
      s.zipWithIndex
        .sliding(numDistinct)
        .find(_.map(_._1).toSet.size == numDistinct)
        .mapConcat(_.lastOption.map(_._2 + 1))
        .runHead

    if (Test) {
      ZIO.foreach(testInputs.map(ZStream.fromIterable(_)))(getSolution).map(_.flatten.mkString("\n"))
    } else
      getSolution(data)
  }

  private lazy val part1 = app(4)

  private lazy val part2 = app(14)

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (if (Part1) part1 else part2).flatMap(ZIO.debug(_))
}
