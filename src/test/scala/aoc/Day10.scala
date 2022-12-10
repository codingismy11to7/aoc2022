package aoc

import com.softwaremill.quicklens._

object Day10 extends AdventDay {
  override final val day = 10

  private sealed trait Instruction
  private object Instruction {
    private val add = """^addx (-?\d+)$""".r
    def apply(line: String): Instruction = line match {
      case "noop" => Noop
      case add(v) => AddX(v.toInt)
      case _      => sys.error(s"invalid line $line")
    }
    case object Noop            extends Instruction
    case class AddX(value: Int) extends Instruction
  }

  private final case class State(
      register: Int = 1,
      counter: Int = 0,
      sumOfRegister: Int = 0,
      complete: Boolean = false,
  )

  override def part1TestExpectation: Any = 13_140
  override def part1Expectation: Any     = 16_880

  override def part1(dataFile: String): STask[Any] = {
    import Instruction._

    val getValueAt = (20 to 220 by 40).toList
    resourceLines(dataFile)
      .runFoldWhile(State() -> getValueAt)(!_._1.complete) {
        case ((state, Nil), _) => state.copy(complete = true) -> Nil

        case ((state, getValueAt), line) =>
          val inst = Instruction(line)
          def incrementState(args: (State, List[Int])) = {
            val (state, getValueAt) = args
            val cycle               = 1 + state.counter
            val (newSum, newGetValueAt) = getValueAt match {
              case x :: xs if x == cycle => (state.sumOfRegister + (cycle * state.register)) -> xs
              case xs                    => state.sumOfRegister                              -> xs
            }
            state.copy(counter = cycle, sumOfRegister = newSum) -> newGetValueAt
          }
          inst match {
            case Noop => incrementState(state -> getValueAt)
            case AddX(value) =>
              val (newState, newGetValueAt) = incrementState(incrementState(state -> getValueAt))
              newState.modify(_.register)(_ + value) -> newGetValueAt
          }
      }
      .map(_._1.sumOfRegister)
  }

  override def part2TestExpectation: Any =
    """##..##..##..##..##..##..##..##..##..##..
      |###...###...###...###...###...###...###.
      |####....####....####....####....####....
      |#####.....#####.....#####.....#####.....
      |######......######......######......####
      |#######.......#######.......#######.....""".stripMargin

  override def part2Expectation: Any =
    """###..#..#..##..####..##....##.###..###..
      |#..#.#.#..#..#....#.#..#....#.#..#.#..#.
      |#..#.##...#..#...#..#..#....#.###..#..#.
      |###..#.#..####..#...####....#.#..#.###..
      |#.#..#.#..#..#.#....#..#.#..#.#..#.#.#..
      |#..#.#..#.#..#.####.#..#..##..###..#..#.""".stripMargin

  override def part2(dataFile: String): STask[Any] = {
    import Instruction._

    resourceLines(dataFile)
      .runFoldWhile(State() -> List.empty[Char])(_._1.counter < 240) { case ((state, screen), line) =>
        val inst = Instruction(line)

        def incrementState(args: (State, List[Char])) = {
          val (state, screen) = args
          val pixelOn         = math.abs((state.counter % 40) - state.register) <= 1
          val newScreen       = (if (pixelOn) '#' else '.') :: screen

          val cycle = 1 + state.counter
          state.copy(counter = cycle) -> newScreen
        }

        inst match {
          case Noop => incrementState(state, screen)
          case AddX(value) =>
            val (newState, newScreen) = incrementState(incrementState(state, screen))
            newState.modify(_.register)(_ + value) -> newScreen
        }
      }
      .map(_._2.reverse.mkString.grouped(40).mkString("\n"))
  }
}
