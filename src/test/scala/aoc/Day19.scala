package aoc

import com.softwaremill.quicklens._
import enumeratum._
import zio.{Chunk, ZIO}

object Day19 extends AdventDay {
  override final val day = 19

  private final case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geodes: Int = 0) {
    def +(o: Resources): Resources = Resources(ore + o.ore, clay + o.clay, obsidian + o.obsidian, geodes + o.geodes)
    def -(o: Resources): Resources = Resources(ore - o.ore, clay - o.clay, obsidian - o.obsidian, geodes - o.geodes)

    def canBuildGiven(o: Resources): Boolean = ore <= o.ore && clay <= o.clay && obsidian <= o.obsidian

    def eachMax(o: Resources): Resources = Resources(
      math.max(ore, o.ore),
      math.max(clay, o.clay),
      math.max(obsidian, o.obsidian),
      math.max(geodes, o.geodes),
    )
  }

  private final case class Blueprint(
      id: Int,
      oreCost: Resources,
      clayCost: Resources,
      obsidianCost: Resources,
      geodeCost: Resources,
  ) {
    private lazy val maxCosts = List(oreCost, clayCost, obsidianCost, geodeCost).foldLeft(Resources())(_ eachMax _)

    def validNextStates(currentRobots: Resources, currentResources: Resources): Chunk[(Robot, Resources, Resources)] =
      Chunk(
        Option.when((oreCost canBuildGiven currentResources) && currentRobots.ore < maxCosts.ore)(
          (Robot.Ore, currentRobots.modify(_.ore)(_ + 1), currentResources - oreCost),
        ),
        Option.when((clayCost canBuildGiven currentResources) && currentRobots.clay < maxCosts.clay)(
          (Robot.Clay, currentRobots.modify(_.clay)(_ + 1), currentResources - clayCost),
        ),
        Option.when((obsidianCost canBuildGiven currentResources) && currentRobots.obsidian < maxCosts.obsidian)(
          (Robot.Obsidian, currentRobots.modify(_.obsidian)(_ + 1), currentResources - obsidianCost),
        ),
        Option.when(geodeCost canBuildGiven currentResources)(
          (Robot.Geode, currentRobots.modify(_.geodes)(_ + 1), currentResources - geodeCost),
        ),
      ).flatten
  }
  private object Blueprint {
    private val re =
      """Blueprint (\d+):.*costs (\d+) ore.*costs (\d+) ore.*costs (\d+) ore and (\d+) clay.*costs (\d+) ore and (\d+) obsidian.""".r
    def parse(line: String): Blueprint = line match {
      case re(i, a, b, c, d, e, f) =>
        Blueprint(
          i.toInt,
          Resources(ore = a.toInt),
          Resources(ore = b.toInt),
          Resources(ore = c.toInt, clay = d.toInt),
          Resources(ore = e.toInt, obsidian = f.toInt),
        )

      case _ => sys.error(s"invalid line $line")
    }
  }

  private sealed trait Robot extends EnumEntry
  private object Robot extends Enum[Robot] {
    val values: IndexedSeq[Robot] = findValues
    case object Ore      extends Robot
    case object Clay     extends Robot
    case object Obsidian extends Robot
    case object Geode    extends Robot
  }

  private final case class State(collectedResources: Resources, robots: Resources)
  private def runSimulation(blueprint: Blueprint, maxMinutes: Int) = {
    def loop(minute: Int, state: State, doNotBuild: Set[Robot]): Int =
      if (minute > maxMinutes) state.collectedResources.geodes
      else {
        val validStates = blueprint.validNextStates(state.robots, state.collectedResources)

        val possibilities =
          if (validStates.exists(_._1 == Robot.Geode))
            validStates.collect { case (Robot.Geode, a, b) => (Some(Robot.Geode), a, b) }
          else
            (Option.empty[Robot], state.robots, state.collectedResources) +:
              validStates.filterNot(x => doNotBuild.contains(x._1)).map(x => (Some(x._1), x._2, x._3))

        possibilities.map { case (builtOpt, newRobots, newResources) =>
          val newCollectedResources = newResources + state.robots
          val newDoNotBuild         = builtOpt.fold(doNotBuild ++ validStates.map(_._1))(_ => Set.empty)
          loop(1 + minute, state.copy(newCollectedResources, newRobots), newDoNotBuild)
        }.max

      }

    loop(1, State(Resources(), Resources(ore = 1)), Set.empty)
  }

  private def blueprints(dataFile: String) = resourceLines(dataFile).filter(_.nonEmpty).map(Blueprint.parse)

  override def part1TestExpectation: Any = 33
  override def part1Expectation: Any     = 1_306

  override def part1(dataFile: String): STask[Any] = blueprints(dataFile).runCollect.flatMap { bp =>
    ZIO.foreachPar(bp)(bp => ZIO.succeed(runSimulation(bp, 24) * bp.id)).map(_.sum)
  }

  override def part2TestExpectation: Any = 3_472
  override def part2Expectation: Any     = 37_604

  override def part2(dataFile: String): STask[Any] = blueprints(dataFile).take(3).runCollect.flatMap { bp =>
    ZIO.foreachPar(bp)(bp => ZIO.succeed(runSimulation(bp, 32))).map(_.product)
  }
}
