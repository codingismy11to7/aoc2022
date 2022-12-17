package aoc

import com.softwaremill.quicklens.ModifyPimp
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.config.CoreConfig
import scalax.collection.edge.Implicits._
import scalax.collection.edge.WUnDiEdge
import zio._

object Day16 extends AdventDay {
  override final val day = 16

  private type Volcano = Graph[Valve, UnDiEdge]
  private def volcano = ZIO.service[Volcano]

  private final case class Valve(label: String, flowRate: Int)

  private final case class ValveAndTunnels(valve: Valve, tunnelsTo: Set[String])
  private val re = """Valve (.+) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r
  private def graphAndValves(dataFile: String) = resourceLines(dataFile)
    .runFold(Chunk.empty[ValveAndTunnels]) {
      case (acc, re(l, r, t)) => acc :+ ValveAndTunnels(Valve(l, r.toInt), t.split("""\s*,\s*""").toSet)
      case (_, line)          => sys.error(s"invalid line $line")
    }
    .map { valves =>
      val valvesByLabel            = valves.map(v => v.valve.label -> v.valve).toMap
      implicit val c: Graph.Config = CoreConfig()
      val b                        = Graph.newBuilder[Valve, UnDiEdge]
      valves.foreach(v => b ++= v.tunnelsTo.map(l => valvesByLabel(l) ~ v.valve))
      b.result() -> valves.map(_.valve)
    }

  private final case class State(remMinutes: Int, willBeReleased: Int, opened: Chunk[Valve])

  private def remMinutesAfterGettingToAndOpening(from: Valve, to: Valve, state: State) =
    if (state.remMinutes <= 0) ZIO.succeed(None)
    else
      volcano.map { v =>
        v.get(from).shortestPathTo(v.get(to)).map(_.nodes).collect {
          case p if p.size <= state.remMinutes => state.modify(_.remMinutes)(_ - p.size)
        }
      }

  private def findLargestRelease(
      at: Valve,
      unopened: Set[Valve],
      state: State,
  ): URIO[Volcano, State] =
    ZIO
      .foreachPar(unopened)(to => remMinutesAfterGettingToAndOpening(at, to, state).map(_ -> to))
      .map(_.collect { case (Some(s), v) =>
        s.modify(_.willBeReleased)(_ + s.remMinutes * v.flowRate).modify(_.opened)(_ :+ v) -> v
      })
      .flatMap { possiblePaths =>
        if (possiblePaths.isEmpty) ZIO.succeed(state)
        else {
          ZIO
            .foreachPar(possiblePaths) { case (s, v) =>
              findLargestRelease(v, unopened - v, s)
            }
            .map(_.maxBy(_.willBeReleased))
        }
      }

  override def part1TestExpectation: Any = 1651
  override def part1Expectation: Any     = 2183

  override def part1(dataFile: String): STask[Any] = graphAndValves(dataFile).flatMap { case (v, valves) =>
    val unopened = valves.filter(_.flowRate > 0).toSet
    val at       = valves.find(_.label == "AA").getOrElse(sys.error("Couldn't find starting valve"))
    findLargestRelease(at, unopened, State(30, 0, Chunk.empty)).provide(ZLayer.succeed(v)).map(_.willBeReleased)
  }

  private def graphsAndValves(dataFile: String) = graphAndValves(dataFile).map { case (v, valves) =>
    implicit val c: CoreConfig = CoreConfig()
    val toGraph                = valves.filter(v => v.flowRate > 0 || v.label == "AA")
    val newGB                  = Graph.newBuilder[Valve, WUnDiEdge]
    for {
      a <- toGraph
      b <- toGraph
      if a != b
      path <- v.get(a).shortestPathTo(v.get(b))
      w     = path.nodes.size
    } newGB += (a ~ b % w)
    (v, newGB.result(), valves)
  }

  private final case class MovementAndOpening(from: Valve, to: Valve, time: Int)
  private final case class Path(maxTime: Int, moves: Chunk[MovementAndOpening] = Chunk.empty) {
    def append(move: MovementAndOpening): Path = copy(moves = moves :+ move)

    lazy val valid: Boolean = moves.map(_.time).sum <= maxTime
    lazy val released: Int = moves
      .foldLeft(0 -> maxTime) { case ((accPressure, timeRem), move) =>
        val newTimeRem  = timeRem - move.time
        val newPressure = accPressure + newTimeRem * move.to.flowRate
        newPressure -> newTimeRem
      }
      ._1

    lazy val containedValves: Set[Valve] = moves.flatMap(m => Set(m.from, m.to)).toSet
  }

  override def part2TestExpectation: Any = 1707
  override def part2Expectation: Any     = 2911

  override def part2(dataFile: String): STask[Any] = graphsAndValves(dataFile).flatMap { case (_, w, valves) =>
    val unopened = valves.filter(_.flowRate > 0)
    val aa       = valves.find(_.label == "AA").getOrElse(sys.error("Couldn't find starting valve"))

    def createMovementAndOpening(from: Valve, to: Valve) = {
      val time = w.get(from).shortestPathTo(w.get(to)).get.edges.head.weight.toInt
      MovementAndOpening(from, to, time)
    }

    def allPaths(from: Valve, unopened: Chunk[Valve], preceedingPath: Path): UIO[Chunk[Path]] =
      if (unopened.isEmpty || !preceedingPath.valid) ZIO.succeed(Chunk.empty)
      else
        ZIO
          .foreachPar(unopened) { toOpen =>
            val thisMove = createMovementAndOpening(from, toOpen)
            val newPath  = preceedingPath append thisMove
            allPaths(toOpen, unopened.filterNot(_ == toOpen), newPath).map(newPath +: _)
          }
          .map(_.flatten)

    (ZIO.debug("calculating all paths").when(false) *> allPaths(aa, unopened, Path(26))).flatMap { ps =>
//      println(s"got ${ps.size} paths, getting valid")
      val valid = ps.filter(_.valid)
      val justA = Set(aa)
//      println(s"got ${valid.size} valid, calculating scores")
      ZIO
        .foreachPar(valid) { a =>
          ZIO.succeed {
            for {
              b  <- valid
              int = a.containedValves & b.containedValves
              if int == justA
            } yield a.released + b.released
          }
        }
        .map(_.flatten[Int])
        .map { scores =>
//          println("got scores, getting highest")
          scores.max
        }
    }
  }
}
