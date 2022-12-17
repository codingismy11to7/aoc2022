package aoc

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

  override def part2TestExpectation: Any           = 1_514_285_714_288L
  override def part2(dataFile: String): STask[Any] = ZIO.succeed(0)
}
