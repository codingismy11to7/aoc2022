package aoc

import enumeratum._
import zio._

object Day2 extends ZIOAppDefault {
  final val Test  = false
  final val Part1 = false

  private def scoreFor(rps: RPS)         = 1 + RPS.values.indexOf(rps)
  private def scoreFor(outcome: Outcome) = 3 * Outcome.values.indexOf(outcome)
  private def getMyOutcome(myPlay: RPS, theirPlay: RPS) =
    myPlay -> theirPlay match {
      case (RPS.Scissors, RPS.Rock) => Outcome.Lose
      case (RPS.Rock, RPS.Scissors) => Outcome.Win
      case _ =>
        val myIdx    = RPS.values.indexOf(myPlay)
        val theirIdx = RPS.values.indexOf(theirPlay)
        if (myIdx > theirIdx) Outcome.Win
        else if (theirIdx > myIdx) Outcome.Lose
        else Outcome.Draw
    }
  private def getTheirPlay(char: Char) = RPS.values(char - 'A')
  private def getMyPlay(char: Char)    = RPS.values(char - 'X')

  /* part 2 */
  private def neededOutcome(char: Char) = Outcome.values(char - 'X')
  private def getMyPlay(theirPlay: RPS, neededOutcome: Outcome) =
    neededOutcome match {
      case Outcome.Draw                          => theirPlay
      case Outcome.Win                           => RPS.values((RPS.values.indexOf(theirPlay) + 1) % 3)
      case Outcome.Lose if theirPlay == RPS.Rock => RPS.Scissors
      case Outcome.Lose                          => RPS.values(RPS.values.indexOf(theirPlay) - 1)
    }

  final case class Play(myPlay: RPS, theirPlay: RPS) { self =>
    lazy val myScore: Int  = scoreFor(myPlay) + scoreFor(getMyOutcome(myPlay, theirPlay))
    val doDebug: UIO[Unit] = ZIO.succeed(s"Score $myScore for $self").flatMap(ZIO.debug(_))
  }
  object Play {
    type Parser = String => STask[Play]

    private val lineRe = """([A-C]) ([X-Z])""".r

    private def parse(f: (Char, Char) => STask[Play])(line: String) = line match {
      case lineRe(t, m) => f(m.charAt(0), t.charAt(0))
      case _            => ZIO.fail(s"Invalid line: $line")
    }

    def createPart1: Parser =
      parse((m, t) =>
        for {
          mp <- attempt(getMyPlay(m))
          tp <- attempt(getTheirPlay(t))
        } yield Play(mp, tp),
      )

    def createPart2: Parser =
      parse((m, t) =>
        for {
          no <- attempt(neededOutcome(m))
          tp <- attempt(getTheirPlay(t))
        } yield Play(getMyPlay(tp, no), tp),
      )
  }

  sealed trait Outcome extends EnumEntry
  object Outcome extends Enum[Outcome] {
    val values: IndexedSeq[Outcome] = findValues
    case object Lose extends Outcome
    case object Draw extends Outcome
    case object Win  extends Outcome
  }

  sealed trait RPS extends EnumEntry
  object RPS extends Enum[RPS] {
    val values: IndexedSeq[RPS] = findValues
    case object Rock     extends RPS
    case object Paper    extends RPS
    case object Scissors extends RPS
  }

  private val data = resourceLines(s"2/${if (Test) "test" else "input"}.txt").filterNot(_.isEmpty)

  private def app(f: Play.Parser) = {
    val plays = data.mapZIO(f).tap(_.doDebug)
    val total = plays.map(_.myScore).runSum
    total.map(t => s"My total is $t")
  }

  private val doPart1 = app(Play.createPart1)
  private val doPart2 = app(Play.createPart2)

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    (if (Part1) doPart1 else doPart2).flatMap(ZIO.debug(_)).exitCode
}
