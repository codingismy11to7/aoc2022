package aoc

import zio.{Chunk, ZIO}
import zio.json._
import zio.json.ast.Json

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object Day13 extends AdventDay {
  override final val day = 13

  private final case class Pair(index: Long, left: Json, right: Json)
  private def parseJson(s: String) = ZIO.fromEither(s.fromJson[Json])

  private def lessThanOpt(left: Json, right: Json): Option[Boolean] = left -> right match {
    case (l: Json.Arr, r: Json.Num)          => lessThanOpt(l, Json.Arr(r))
    case (l: Json.Num, r: Json.Arr)          => lessThanOpt(Json.Arr(l), r)
    case (Json.Num(l), Json.Num(r)) if l < r => Some(true)
    case (Json.Num(l), Json.Num(r)) if r < l => Some(false)
    case (_: Json.Num, _: Json.Num)          => None

    case (Json.Arr(lefts), Json.Arr(rights)) =>
      @tailrec
      def loop(remL: Chunk[Json] = lefts, remR: Chunk[Json] = rights): Option[Boolean] =
        if (remL.isEmpty && remR.isEmpty) None
        else if (remL.isEmpty) Some(true)
        else if (remR.isEmpty) Some(false)
        else {
          val left  = remL.head
          val right = remR.head
          lessThanOpt(left, right) match {
            case ans @ Some(_) => ans
            case None          => loop(remL.tail, remR.tail)
          }
        }
      loop()

    case _ => sys.error("should only have arrays and numbers")
  }
  private def lessThan(left: Json, right: Json) =
    lessThanOpt(left, right).getOrElse(sys.error(s"$left/$right couldn't be ordered"))
  private def pairInCorrectOrder(p: Pair) = lessThan(p.left, p.right)

  override def part1TestExpectation: Any = 13
  override def part1Expectation: Any     = 5_003

  override def part1(dataFile: String): STask[Any] = {
    val lines      = resourceLines(dataFile)
    val chunks     = lines.split(_.isEmpty).tap(cs => ZIO.fail(s"Not 2 chunks! $cs").unless(cs.size == 2))
    val jsonChunks = chunks.mapZIO(ZIO.foreachPar(_)(parseJson))
    val pairs      = jsonChunks.zipWithIndex.map { case (js, idx) => Pair(1 + idx, js(0), js(1)) }
    pairs.filter(pairInCorrectOrder).map(_.index).runSum
  }

  override def part2TestExpectation: Any = 140
  override def part2Expectation: Any     = 20_280

  override def part2(dataFile: String): STask[Any] = {
    def binaryIndexOf(needle: Json, sortedHaystack: Chunk[Json]) = {
      @tailrec
      def loop(startIdx: Int, endIdx: Int): Int = {
        val mid    = startIdx + (endIdx - startIdx) / 2
        val compTo = sortedHaystack(mid)
        lessThanOpt(needle, compTo) match {
          case None       => mid
          case Some(true) => loop(startIdx, mid - 1)
          case _          => loop(mid + 1, endIdx)
        }
      }
      loop(0, sortedHaystack.size - 1)
    }

    for {
      jsons      <- resourceLines(dataFile).filterNot(_.isEmpty).mapZIO(parseJson).runCollect
      div1       <- parseJson("[[2]]")
      div2       <- parseJson("[[6]]")
      sortedJsons = (jsons ++ Chunk(div1, div2)).sortWith(lessThan)
      idx1        = 1 + binaryIndexOf(div1, sortedJsons)
      idx2        = 1 + binaryIndexOf(div2, sortedJsons)
    } yield idx1 * idx2
  }
}
