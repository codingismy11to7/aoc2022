package aoc

import com.softwaremill.quicklens._
import zio._

import scala.annotation.tailrec

object Day11 extends AdventDay {
  override final val day = 11

  private sealed trait Value { def evaluate(oldValue: BigInt): BigInt }
  private final case object OldValue     extends Value { override def evaluate(oldValue: BigInt): BigInt = oldValue  }
  private final case class Const(v: Int) extends Value { override def evaluate(oldValue: BigInt): BigInt = BigInt(v) }
  private object Value {
    def apply(s: String): Value = s match {
      case "old"  => OldValue
      case digits => Const(digits.toInt)
    }
  }
  private sealed trait Op { def evaluate(value1: BigInt, value2: BigInt): BigInt }
  private case object Plus extends Op {
    override def evaluate(value1: BigInt, value2: BigInt): BigInt = value1 + value2
  }
  private case object Times extends Op {
    override def evaluate(value1: BigInt, value2: BigInt): BigInt = value1 * value2
  }
  private final case class Expr(value1: Value, op: Op, value2: Value) {
    def evaluate(oldValue: BigInt): BigInt = op.evaluate(value1.evaluate(oldValue), value2.evaluate(oldValue))
  }
  private object Expr {
    private val re = """(old|\d+) ([*+]) (old|\d+)""".r
    def apply(s: String): Expr = s match {
      case re(v1, o, v2) =>
        val op = o match { case "+" => Plus; case "*" => Times }
        Expr(Value(v1), op, Value(v2))

      case _ => sys.error(s"Invalid expression $s")
    }
  }

  private final case class Monkey(
      id: Int,
      itemWorryLevels: Chunk[BigInt],
      op: Expr,
      testDivisibleBy: Int,
      trueThrowTo: Int,
      falseThrowTo: Int,
  )
  private object Monkey {
    def create(lines: Chunk[String]): Monkey = {
      val id = lines(0) match { case s"Monkey $id:" => id.toInt }
      val items = lines(1) match {
        case s"  Starting items: $items" => Chunk.fromArray(items.split(""",\s*""")).map(BigInt(_))
      }
      val op  = lines(2) match { case s"  Operation: new = $op" => Expr(op) }
      val tdb = lines(3) match { case s"  Test: divisible by $db" => db.toInt }
      val ttt = lines(4) match { case s"    If true: throw to monkey $m" => m.toInt }
      val ftt = lines(5) match { case s"    If false: throw to monkey $m" => m.toInt }
      Monkey(id, items, op, tdb, ttt, ftt)
    }
  }

  private def monkeys(dataFile: String) = resourceLines(dataFile).split(_.trim.isEmpty).map(Monkey.create).runCollect

  private sealed trait WorryModifier { def modify(worry: BigInt): BigInt }
  private final case object DivideByThree extends WorryModifier {
    override def modify(worry: BigInt): BigInt = worry / 3
  }
  private final case class Modulo(by: BigInt) extends WorryModifier {
    override def modify(worry: BigInt): BigInt = worry % by
  }

  private def app(modifier: Chunk[Monkey] => WorryModifier, rounds: Int)(dataFile: String): STask[Any] =
    monkeys(dataFile).map { ms =>
      val worryMod = modifier(ms)
      def inspectFirstItem(monkeyId: Int, monkeys: Chunk[Monkey], inspectionCounts: Map[Int, BigInt]) = {
        val monkey = monkeys(monkeyId)
        monkey.itemWorryLevels.headOption.map { currItem =>
          val newInspectCounts =
            inspectionCounts + (monkeyId -> (inspectionCounts.getOrElse(monkeyId, BigInt(0)) + BigInt(1)))
          val worryAfterOp      = monkey.op.evaluate(currItem)
          val worryAfterBoredom = worryMod.modify(worryAfterOp)
          val isDivisible       = (worryAfterBoredom % monkey.testDivisibleBy) == 0
          val throwTo           = if (isDivisible) monkey.trueThrowTo else monkey.falseThrowTo
          val thisMonkey        = monkey.modify(_.itemWorryLevels)(_.tail)
          val targetMonkey      = monkeys(throwTo).modify(_.itemWorryLevels)(_ :+ worryAfterBoredom)
          val newMonkeys        = monkeys.updated(thisMonkey.id, thisMonkey).updated(targetMonkey.id, targetMonkey)
          newMonkeys -> newInspectCounts
        }
      }

      def inspectAllItems(monkeyId: Int, monkeys: Chunk[Monkey], inspectionCounts: Map[Int, BigInt]) = {
        @tailrec
        def loop(
            accMonkeys: Chunk[Monkey] = monkeys,
            accInspectionCounts: Map[Int, BigInt] = inspectionCounts,
        ): (Chunk[Monkey], Map[Int, BigInt]) =
          inspectFirstItem(monkeyId, accMonkeys, accInspectionCounts) match {
            case None            => accMonkeys -> accInspectionCounts
            case Some((ms, ics)) => loop(ms, ics)
          }
        loop()
      }

      def runRound(monkeys: Chunk[Monkey], inspectionCounts: Map[Int, BigInt]) =
        monkeys.indices.foldLeft(monkeys -> inspectionCounts) { case ((accMonkeys, accInspections), monkeyId) =>
          inspectAllItems(monkeyId, accMonkeys, accInspections)
        }

      val (_, counts) = (1 to rounds).foldLeft(ms -> Map.empty[Int, BigInt]) { case ((accMonkeys, accInspections), _) =>
        runRound(accMonkeys, accInspections)
      }

      val topTwo = counts.values.toSeq.sorted.reverse.take(2)

      topTwo.product
    }

  override def part1TestExpectation: Any           = 10_605
  override def part1Expectation: Any               = 54_752
  override def part1(dataFile: String): STask[Any] = app(_ => DivideByThree, 20)(dataFile)

  private def createModulo(ms: Chunk[Monkey])      = Modulo(ms.map(_.testDivisibleBy).map(BigInt(_)).product)
  override def part2TestExpectation: Any           = 2_713_310_158L
  override def part2Expectation: Any               = 13_606_755_504L
  override def part2(dataFile: String): STask[Any] = app(createModulo, 10_000)(dataFile)
}
