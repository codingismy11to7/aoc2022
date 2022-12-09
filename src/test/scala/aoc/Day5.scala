package aoc

import zio._

import scala.collection.immutable.ListMap

object Day5 extends AdventDay {
  override final val day = 5

  private sealed trait CrateSpot
  private object CrateSpot {
    private val crateRe = """^\[(.)]\s?$""".r
    private val ws      = """^\s+$""".r
    private val label   = """^\s(.)\s*$""".r
    def parseLine(line: String): Chunk[CrateSpot] = Chunk.fromIterator {
      line.grouped(4).map {
        case crateRe(label) => Crate(label.head)
        case ws()           => Empty
        case label(label)   => StackLabel(label.head)
        case _              => sys.error(s"invalid line $line")
      }
    }
    case object Empty                  extends CrateSpot
    case class Crate(label: Char)      extends CrateSpot
    case class StackLabel(label: Char) extends CrateSpot
  }
  private type Crate      = Char
  private type StackLabel = Char
  private type Stacks     = ListMap[StackLabel, List[Crate]]

  private final case class ParsingState(
      parsingDrawing: Boolean = true,
      parsedDrawings: List[Chunk[CrateSpot]] = Nil,
      stackState: Stacks = ListMap.empty,
  ) {
    def parseDrawingLine(line: String): ParsingState =
      copy(parsedDrawings = CrateSpot.parseLine(line) :: parsedDrawings)

    def finishedParsingDrawing: ParsingState = {
      val labels                 = parsedDrawings.head.collect { case CrateSpot.StackLabel(label) => label }
      val initStackState: Stacks = ListMap.empty ++ labels.map(_ -> Nil)
      val stackState = parsedDrawings.tail.foldLeft(initStackState) { case (accState, currLine) =>
        currLine.zipWithIndex.foldLeft(accState) {
          case (state, (CrateSpot.Crate(label), idx)) =>
            val stackLabel = labels(idx)
            val newStack   = label :: state(stackLabel)
            state + (stackLabel -> newStack)

          case (state, _) => state
        }
      }
      ParsingState(parsingDrawing = false, stackState = stackState)
    }

    val processMovePart1: Move => ParsingState = { case Move(from, to, count) =>
      def doMove(stackState: Stacks) = {
        val toMove :: restFromStack = stackState(from)
        val newToStack              = toMove :: stackState(to)
        stackState ++ Map(from -> restFromStack, to -> newToStack)
      }

      val newState = (1 to count).foldLeft(stackState) { case (s, _) => doMove(s) }

      copy(stackState = newState)
    }

    val processMovePart2: Move => ParsingState = { case Move(from, to, count) =>
      val (toMove, newFrom) = stackState(from).splitAt(count)
      val newTo             = toMove ++ stackState(to)
      val newState          = stackState ++ Map(from -> newFrom, to -> newTo)
      copy(stackState = newState)
    }
  }

  private final case class Move(from: StackLabel, to: StackLabel, count: Int)
  private object Move {
    private val re = """^move (\d+) from (.) to (.)$""".r
    def parseLine(line: String): Move = line match {
      case re(count, from, to) => Move(from.head, to.head, count.toInt)
      case _                   => sys.error(s"invalid line $line")
    }
  }

  private def app(dataFile: String)(moveProcessor: ParsingState => Move => ParsingState) =
    resourceLines(dataFile)
      .runFold(ParsingState()) {
        case (state, line) if state.parsingDrawing && line.isEmpty => state.finishedParsingDrawing
        case (state, line) if state.parsingDrawing                 => state.parseDrawingLine(line)
        case (state, line)                                         => moveProcessor(state)(Move.parseLine(line))
      }
      .map(_.stackState.values.map(_.head).mkString)

  override def part1TestExpectation: Any           = "CMZ"
  override def part1Expectation: Any               = "VPCDMSLWJ"
  override def part1(dataFile: String): STask[Any] = app(dataFile)(_.processMovePart1)

  override def part2TestExpectation: Any           = "MCD"
  override def part2Expectation: Any               = "TPWCGNCCG"
  override def part2(dataFile: String): STask[Any] = app(dataFile)(_.processMovePart2)
}
