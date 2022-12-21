package aoc

import zio.{UIO, ZIO}

import scala.annotation.tailrec

object Day21 extends AdventDay {
  override final val day = 21

  private sealed trait Expr
  private object Expr {
    final case class Const(n: Int) extends Expr
    final case class Op(a: String, op: Char, b: String) extends Expr {
      def apply(a: Long, b: Long): Long = op match {
        case '+' => a + b
        case '-' => a - b
        case '*' => a * b
        case '/' => a / b
      }

      def needAnswer(ans: Long, aVal: Option[Long], bVal: Option[Long]): Long = op match {
        case '+' => ans - aVal.getOrElse(0L) - bVal.getOrElse(0L)

        case '-' =>
          aVal -> bVal match {
            case (Some(a), None) => a - ans
            case (None, Some(b)) => ans + b
            case o               => sys.error(s"can't find ans $ans for $op from $o")
          }

        case '*' => ans / aVal.getOrElse(1L) / bVal.getOrElse(1L)

        case '/' =>
          aVal -> bVal match {
            case (Some(a), None) => a / ans
            case (None, Some(b)) => ans * b
            case o               => sys.error(s"can't find ans $ans for $op from $o")
          }
      }
    }

    private val const = """(\d+)""".r
    private val op    = """(.{4}) (.) (.{4})""".r
    def parse(expr: String): Expr = expr match {
      case const(n)     => Const(n.toInt)
      case op(a, op, b) => Op(a, op.head, b)
    }
  }
  private final case class Line(label: String, expr: Expr)
  private object Line {
    private val re = """(.{4}): (.*)""".r
    def parse(line: String): Line = line match {
      case re(a, b) => Line(a, Expr.parse(b))
    }
  }

  private def linesByLabel(dataFile: String) =
    resourceLines(dataFile).filterNot(_.isEmpty).map(Line.parse).runCollect.map(_.map(l => l.label -> l).toMap)

  private def resolve(lines: Map[String, Line])(label: String): UIO[Long] =
    lines(label).expr match {
      case Expr.Const(n) => ZIO succeed n.toLong

      case op @ Expr.Op(a, _, b) =>
        for {
          a <- resolve(lines)(a)
          b <- resolve(lines)(b)
        } yield op(a, b)
    }

  override def part1TestExpectation: Any = 152
  override def part1Expectation: Any     = 41_857_219_607_906L

  override def part1(dataFile: String): STask[Any] = linesByLabel(dataFile).flatMap { byLabel =>
    resolve(byLabel)("root")
  }

  override def part2TestExpectation: Any = 301
  override def part2Expectation: Any     = 3_916_936_880_448L

  override def part2(dataFile: String): STask[Any] = linesByLabel(dataFile).flatMap { byLabel =>
    def containsHumn(label: String) = {
      @tailrec
      def loop(toCheck: Set[String]): Boolean =
        if (toCheck.isEmpty) false
        else {
          val curr = toCheck.head
          if (curr == "humn") true
          else {
            val rest     = toCheck.tail
            val currLine = byLabel(curr)
            currLine.expr match {
              case _: Expr.Const    => loop(rest)
              case Expr.Op(a, _, b) => loop(rest ++ Set(a, b))
            }
          }
        }
      loop(Set(label))
    }

    def findHumnValue(forLabel: String)(neededAnswer: Long): STask[Long] =
      if (forLabel == "humn")
        ZIO.succeed(neededAnswer)
      else
        ZIO.succeed(byLabel(forLabel)).flatMap {
          case Line(_, op @ Expr.Op(a, _, b)) =>
            if (containsHumn(a))
              resolve(byLabel)(b).flatMap { bVal =>
                findHumnValue(a)(op.needAnswer(neededAnswer, None, Some(bVal)))
              }
            else
              resolve(byLabel)(a).flatMap { aVal =>
                findHumnValue(b)(op.needAnswer(neededAnswer, Some(aVal), None))
              }

          case const => ZIO.fail(s"findHumnValue called on $const")
        }

    val Line(_, Expr.Op(a, _, b)) = byLabel("root")
    if (containsHumn(a))
      resolve(byLabel)(b).flatMap(findHumnValue(a))
    else
      resolve(byLabel)(a).flatMap(findHumnValue(b))
  }

}
