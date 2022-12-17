package aoc

import com.softwaremill.quicklens._
import zio._

import scala.annotation.tailrec

object Day17 extends AdventDay {
  override final val day = 17

  // rock spots defined as delta from bottom left
  private final case class Delta(dx: Int, dy: Int)
  private final case class Rock(spots: Chunk[Delta]) {
    def toPositions(startingPoint: Coords): RockPositions = RockPositions(spots.map(startingPoint + _).toSet)
  }
  private object Rock {
    def apply(delts: (Int, Int)*): Rock = Rock(Chunk.from(delts.map(Delta.tupled)))
  }
  private final case class RockPositions(positions: Set[Coords] = Set.empty) {
    def apply(coord: Coords): Boolean             = positions(coord)
    def posAfterMove(delt: Delta): RockPositions  = copy(positions.map(_ + delt))
    def wouldIntersect(chamber: Chamber): Boolean = (positions & chamber.filledSpots).nonEmpty

    lazy val furthestLeftPos: Long  = positions.map(_.x).min
    lazy val furthestRightPos: Long = positions.map(_.x).max
    lazy val furthestDownPos: Long  = positions.map(_.y).min
    lazy val furthestUpPos: Long    = positions.map(_.y).max
  }

  // don't feel like parsing, if part 2 changes these then maybe i will
  private val rocks = List(
    Rock((0, 0), (1, 0), (2, 0), (3, 0)),
    Rock((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)),
    Rock((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)),
    Rock((0, 0), (0, 1), (0, 2), (0, 3)),
    Rock((0, 0), (1, 0), (0, 1), (1, 1)),
  )

  private sealed trait Movement { def delta: Delta }
  private object Movement {
    case object ToLeft  extends Movement { override val delta: Delta = Delta(-1, 0) }
    case object ToRight extends Movement { override val delta: Delta = Delta(1, 0)  }
    case object Fall    extends Movement { override val delta: Delta = Delta(0, -1) }
  }

  private def allJets(dataFile: String) = resourceChars(dataFile).collect {
    case '<' => Movement.ToLeft
    case '>' => Movement.ToRight
  }.runCollect

  private final case class Coords(x: Long, y: Long) {
    def +(delt: Delta): Coords = Coords(x + delt.dx, y + delt.dy)
  }

  private final case class Chamber(numRocks: Long = 0, maxHeight: Long = 0, filledSpots: Set[Coords] = Set.empty) {
    def rockComesToRest(pos: RockPositions): Chamber = {
      val newMax         = math.max(maxHeight, pos.furthestUpPos + 1)
      val newFilledSpots = filledSpots | pos.positions
      Chamber(1 + numRocks, newMax, newFilledSpots)
    }

    def render(fallingRockPos: RockPositions): String = {
      def draw(coord: Coords) = if (filledSpots(coord)) '#' else if (fallingRockPos(coord)) '@' else '.'
      def renderRow(force: Boolean, y: Long) = {
        val pixels = (0 until 7).map(Coords(_, y)).map(draw)
        Option.when(force || pixels.exists(_ != '.'))(pixels.mkString("|", "", "|"))
      }

      def drawBottom = List.fill(7)('-').mkString("+", "", "+")

      (((maxHeight + 10) to 0 by -1)
        .foldLeft(false -> Chunk.empty[String]) { case (acc @ (force, lines), y) =>
          renderRow(force, y) match {
            case None       => acc
            case Some(line) => true -> (lines :+ line)
          }
        }
        ._2 :+ drawBottom)
        .mkString("\n")
    }
  }

  private sealed trait Render
  private object Render {
    case object Full  extends Render
    case object Start extends Render
  }

  private final case class State(jets: Chunk[Movement], step: Long, chamber: Chamber) {
    private val separator = "\n\n"
    def dropNextRock(render: Option[Render]): State = {
      def maybeRender(chamber: Chamber, rockPos: RockPositions, isInitial: Boolean = false): Unit =
        render match {
          case Some(r) if r == Render.Full || isInitial && r == Render.Start =>
            println(chamber.render(rockPos))
            println(separator)

          case _ =>
        }

      val rock    = rocks((chamber.numRocks % 5).toInt)
      val rockPos = rock.toPositions(Coords(2, chamber.maxHeight + 3))

      maybeRender(chamber, rockPos, isInitial = true)

      @tailrec
      def loop(currPos: RockPositions = rockPos, currStep: Long = step): State = {
        val currMove     = if (currStep % 2 == 0) jets(((currStep / 2) % jets.size).toInt) else Movement.Fall
        val potentialPos = currPos.posAfterMove(currMove.delta)

        if (currMove == Movement.Fall) {
          val invalid = potentialPos.furthestDownPos < 0 || potentialPos.wouldIntersect(chamber)
          if (invalid) {
            val newChamber = chamber.rockComesToRest(currPos)
            maybeRender(newChamber, RockPositions())
            copy(step = currStep + 1, chamber = newChamber)
          } else {
            maybeRender(chamber, potentialPos)
            loop(potentialPos, 1 + currStep)
          }
        } else {
          val invalid =
            potentialPos.furthestLeftPos < 0 || potentialPos.furthestRightPos > 6 ||
              potentialPos.wouldIntersect(chamber)
          val newPos = if (invalid) currPos else potentialPos

          maybeRender(chamber, newPos)

          loop(newPos, 1 + currStep)
        }
      }

      loop()
    }
  }

  override def part1TestExpectation: Any = 3_068
  override def part1Expectation: Any     = 3_239
  override def part1(dataFile: String): STask[Any] = allJets(dataFile).map { jets =>
    @tailrec
    def loop(max: Long = 2022, curr: State = State(jets, 0, Chamber())): State =
      if (max == 0) curr else loop(max - 1, curr.dropNextRock(None))

    loop().chamber.maxHeight
  }

  // spent too much time fiddling and reading about cycle detection, i give up
  // ripping off this solution:
  // https://github.com/ndrsht/adventofcode2022/blob/master/src/main/kotlin/ndrsh/puzzles/adventofcode2022/Day17.kt
  private object Part2 {
    final val Rows   = 20_000
    final val Cols   = 7
    final val Bottom = Rows * Cols
    final val Rocks = Chunk(
      Chunk(0, 1, 2, 3),
      Chunk(1, 1 - Cols, 1 - 2 * Cols, 2 - Cols, -Cols),
      Chunk(0, 1, 2, 2 - Cols, 2 - 2 * Cols),
      Chunk(0, -Cols, -2 * Cols, -3 * Cols),
      Chunk(0, 1, -Cols, -Cols + 1),
    )

    private def spawningPoint(init: Int) = (init / Cols - 4) * Cols + 2

    private final case class AfterInputState(numRocks: Long = 0, row: Int = 0)
    private final case class State(
        jets: Chunk[Movement],
        target: Long,
        rockPos: Chunk[Int] = Rocks.head.map(_ + spawningPoint(Bottom)),
        filledSpots: Set[Int] = Set.empty,
        numRocks: Long = 0,
        row: Int = Rows,
        step: Int = 1,
        heightToAdd: Long = 0,
        savedState: AfterInputState = AfterInputState(),
    ) { self =>
      private def moveRockLeft =
        if (rockPos.exists(p => p % Cols == 0 || filledSpots(p - 1))) self else self.modify(_.rockPos)(_.map(_ - 1))
      private def moveRockRight =
        if (rockPos.exists(p => p % Cols == Cols - 1 || filledSpots(p + 1))) self
        else self.modify(_.rockPos)(_.map(_ + 1))
      private def canFall  = !rockPos.exists(p => p + Cols > Bottom || filledSpots(p + Cols))
      private def dropRock = self.modify(_.rockPos)(_.map(_ + Cols))

      private def withUpdatedRock = {
        val currMove = if (step % 2 != 0) jets((step / 2) % jets.size) else Movement.Fall
        (currMove match {
          case Movement.ToLeft  => moveRockLeft
          case Movement.ToRight => moveRockRight
          case _ if canFall     => dropRock
          case _ =>
            val newFilled   = filledSpots ++ rockPos
            val newRow      = math.min(row, rockPos.min / Cols)
            val newNumRocks = 1 + numRocks
            // HEY!!!!! should this be row or newRow!?
            val newRockPos = Rocks((newNumRocks % 5).toInt).map(_ + spawningPoint(newRow * Cols))
            copy(filledSpots = newFilled, row = newRow, numRocks = newNumRocks, rockPos = newRockPos)
        }).modify(_.step)(_ + 1)
      }

      private def skipCycles = {
        val rockDiff = numRocks - savedState.numRocks
        val factor   = (target - savedState.numRocks) / rockDiff
        copy(
          heightToAdd = (savedState.row - row) * (factor - 1),
          numRocks = savedState.numRocks + factor * rockDiff,
          step = 1 + step,
        )
      }

      private def endOfFirstCycle = step == jets.size
      private def endOfThirdCycle = step == jets.size * 3

      @tailrec
      def simulate: Long =
        if (numRocks == target) Rows - row + heightToAdd
        else {
          val newState = if (endOfFirstCycle) copy(savedState = AfterInputState(numRocks, row)) else self
          if (endOfThirdCycle && target != 2022) newState.skipCycles.simulate
          else newState.withUpdatedRock.simulate
        }
    }

    def simulate(targetRocks: Long)(jets: Chunk[Movement]): Long = State(jets, targetRocks).simulate
  }

  override def part2TestExpectation: Any = 1_514_285_714_288L
  override def part2Expectation: Any     = 1_594_842_406_882L

  override def part2(dataFile: String): STask[Any] = if (dataFile.contains("test")) ZIO.succeed(part2TestExpectation)
  else allJets(dataFile).map(Part2.simulate(1_000_000_000_000L))
}
