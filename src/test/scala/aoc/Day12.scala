package aoc

import com.softwaremill.quicklens._
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.config.CoreConfig
import zio._

import scala.annotation.tailrec

object Day12 extends AdventDay {
  override final val day = 12

  private final case class Position(x: Int, y: Int)
  private final case class MatrixSpot(char: Char, pos: Position) { val elevation: Int = char - 'a' }

  private final case class Matrix(m: Chunk[Chunk[MatrixSpot]], start: MatrixSpot, end: MatrixSpot) {
    private val rows                              = m.size
    private val cols                              = m.head.size
    private def spotAt(p: Position)               = m(p.y)(p.x)
    private def allSpots                          = m.flatMap(identity)
    private def spotsReachableFrom(s: MatrixSpot) = movesFrom(s.pos).map(spotAt)
    private def movesFrom(p: Position) = {
      def valid(fromPos: Position)(to: Position) =
        to.x >= 0 && to.y >= 0 && to.x < cols && to.y < rows && {
          val from   = spotAt(fromPos)
          val target = spotAt(to)
          (from.elevation + 1) >= target.elevation
        }

      Chunk(p.modify(_.x)(_ + 1), p.modify(_.x)(_ - 1), p.modify(_.y)(_ + 1), p.modify(_.y)(_ - 1)).filter(valid(p))
    }

    lazy val asGraph: Graph[MatrixSpot, DiEdge] = {
      implicit val conf: CoreConfig = Graph.defaultConfig
      allSpots
        .foldLeft(Graph.newBuilder[MatrixSpot, DiEdge]) { case (builder, spot) =>
          builder ++= spotsReachableFrom(spot).map(spot ~> _)
        }
        .result()
    }
  }

  private def matrix(dataFile: String) = resourceLines(dataFile).zipWithIndex
    .runFold(
      (Chunk.empty[Chunk[MatrixSpot]], Option.empty[MatrixSpot], Option.empty[MatrixSpot]),
    ) { case ((accM, accS, accE), (line, rowIdx)) =>
      val chars = Chunk.fromArray(line.split("")).map(_.head)
      val (row, sOpt, eOpt) =
        chars.zipWithIndex.foldLeft((Chunk.empty[MatrixSpot], Option.empty[MatrixSpot], Option.empty[MatrixSpot])) {
          case ((accRow, sOpt, eOpt), (char, colIdx)) =>
            val pos  = Position(colIdx, rowIdx.toInt)
            val elev = char match { case 'S' => 'a'; case 'E' => 'z'; case _ => char }
            val spot = MatrixSpot(elev, pos)
            (accRow :+ spot, if (char == 'S') Some(spot) else sOpt, if (char == 'E') Some(spot) else eOpt)
        }
      (accM :+ row, sOpt.orElse(accS), eOpt.orElse(accE))
    }
    .map {
      case (m, Some(s), Some(e)) => Matrix(m, s, e)
      case x                     => sys.error(s"something went wrong, parsing yielded $x")
    }

  override def part1TestExpectation: Any = Some(31)
  override def part1Expectation: Any     = Some(437)
  override def part1(dataFile: String): STask[Any] = matrix(dataFile).map { matrix =>
    for {
      g    <- Some(matrix.asGraph)
      s    <- g.find(matrix.start)
      e    <- g.find(matrix.end)
      path <- s shortestPathTo e
    } yield path.nodes.size - 1
  }

  override def part2TestExpectation: Any = 29
  override def part2Expectation: Any     = 430
  override def part2(dataFile: String): STask[Any] = matrix(dataFile).map { matrix =>
    val g = matrix.asGraph
    @tailrec
    def loop(
        step: Int = 1,
        prevVisited: Set[MatrixSpot] = Set.empty,
        currToVisit: Set[MatrixSpot] = Set(matrix.end),
    ): Int = {
      val preds =
        currToVisit.flatMap(s => g.find(s).map(_.diPredecessors).getOrElse(Set.empty)).map(node => node: MatrixSpot)
      if (preds.exists(_.elevation == 0)) step
      else {
        val nextVisit = preds &~ prevVisited
        loop(1 + step, prevVisited ++ currToVisit, nextVisit)
      }
    }
    loop()
  }
}
