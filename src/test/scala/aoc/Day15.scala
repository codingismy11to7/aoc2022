package aoc

import zio.stream.{UStream, ZStream}

object Day15 extends AdventDay {
  override final val day = 15

  private final case class Coord(x: Int, y: Int) {
    def nonNegAndLTE(max: Int): Boolean = x >= 0 && y >= 0 && x <= max && y <= max

    def manhattan(that: Coord): Int = math.abs(this.x - that.x) + math.abs(this.y - that.y)

    def allNonNegLimitedCoordsAtDistance(limit: Int, d: Int): UStream[Coord] = {
      val left          = Coord(x - d, y)
      val top           = Coord(x, y - d)
      val right         = Coord(x + d, y)
      val bottom        = Coord(x, y + d)
      val leftToTop     = ZStream.range(1, d).map(i => Coord(left.x + i, left.y - i))
      val topToRight    = ZStream.range(1, d).map(i => Coord(top.x + i, top.y + i))
      val rightToBottom = ZStream.range(1, d).map(i => Coord(right.x - i, right.y + i))
      val bottomToLeft  = ZStream.range(1, d).map(i => Coord(bottom.x - i, bottom.y - i))
      (ZStream(left, top, right, bottom) ++ leftToTop ++ topToRight ++ rightToBottom ++ bottomToLeft)
        .filter(_.nonNegAndLTE(limit))
    }
  }

  private final case class ParsedLine(sensor: Coord, beacon: Coord) {
    def distanceFromSensorToBeacon: Int = sensor manhattan beacon
  }
  private object ParsedLine {
    private val re = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
    def create(line: String): ParsedLine = line match {
      case re(sx, sy, bx, by) => ParsedLine(Coord(sx.toInt, sy.toInt), Coord(bx.toInt, by.toInt))
      case _                  => sys.error(s"invalid line $line")
    }
  }

  private final case class SensorAndDistance(sensor: Coord, distance: Int)

  override def part1TestExpectation: Any = 26
  override def part1Expectation: Any     = 5_878_678

  override def part1(dataFile: String): STask[Any] =
    resourceLines(dataFile)
      .map(ParsedLine.create)
      .runCollect
      .map { lines =>
        val allSensors = lines.map(l => SensorAndDistance(l.sensor, l.distanceFromSensorToBeacon))
        val allBeacons = lines.map(_.beacon).toSet
        def beaconCannotExistAt(c: Coord) =
          if (allBeacons.contains(c)) false
          else if (allSensors.exists(s => s.sensor.manhattan(c) <= s.distance)) true
          else false
        val y = if (dataFile.contains("test")) 10 else 2_000_000
        val (leastX, greatestX) = lines.foldLeft(Int.MaxValue -> Int.MinValue) { case ((accLeast, accMost), line) =>
          val distance = line.distanceFromSensorToBeacon
          val minX     = line.sensor.x - distance
          val maxX     = line.sensor.x + distance
          math.min(accLeast, minX) -> math.max(accMost, maxX)
        }
        (leastX to greatestX).map(x => Coord(x, y)).count(beaconCannotExistAt)
      }

  override def part2TestExpectation: Any = Some(56_000_011)
  override def part2Expectation: Any     = Some(11_796_491_041_245L)

  override def part2(dataFile: String): STask[Any] =
    resourceLines(dataFile).map(ParsedLine.create).runCollect.flatMap { lines =>
      val limit                            = if (dataFile.contains("test")) 20 else 4_000_000
      val allSensors                       = lines.map(l => SensorAndDistance(l.sensor, l.distanceFromSensorToBeacon))
      def outsideOfAllSensors(spot: Coord) = allSensors.forall(s => s.sensor.manhattan(spot) > s.distance)
      val spotsOutsideOfSensors = ZStream.from(allSensors).flatMap { s =>
        s.sensor.allNonNegLimitedCoordsAtDistance(limit, s.distance + 1)
      }

      spotsOutsideOfSensors
        .filter(outsideOfAllSensors)
        .map { spot =>
          spot.x * 4_000_000L + spot.y
        }
        .runHead
    }
}
