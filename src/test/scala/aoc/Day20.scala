package aoc

import zio.Chunk

import scala.annotation.tailrec

object Day20 extends AdventDay {
  override final val day = 20

  private class MutableRing[A](
      val value: A,
      val origIndex: Int,
      val size: Int,
      var prev: MutableRing[A],
      var next: MutableRing[A],
  ) {
    self =>
    def foldLeft[Z](z: Z)(f: (Z, A) => Z): Z = {
      val lastOne = self.prev
      @tailrec
      def loop(acc: Z, curr: MutableRing[A]): Z = {
        val newAcc = f(acc, curr.value)
        if (curr == lastOne) newAcc
        else loop(newAcc, curr.next)
      }
      loop(z, self)
    }

    def foldRight[Z](z: Z)(f: (A, Z) => Z): Z = {
      val lastOne = self
      @tailrec
      def loop(curr: MutableRing[A], acc: Z): Z = {
        val newAcc = f(curr.value, acc)
        if (curr == lastOne) newAcc
        else loop(curr.prev, newAcc)
      }
      loop(self.prev, z)
    }

    def toChunk: Chunk[A] = foldLeft(Chunk.empty[A])(_ :+ _)
    def toList: List[A]   = foldRight(List.empty[A])(_ :: _)

    def foreach(f: A => Unit): Unit = foldLeft(())((_, a) => f(a))

    private def findRing(p: MutableRing[A] => Boolean): Option[MutableRing[A]] = {
      val lastOne = self.prev

      @tailrec
      def loop(curr: MutableRing[A]): Option[MutableRing[A]] =
        if (p(curr)) Some(curr)
        else if (curr == lastOne) None
        else loop(curr.next)

      loop(self)
    }

    def find(v: A): Option[MutableRing[A]]                = findRing(_.value == v)
    def findByOrigIndex(idx: Int): Option[MutableRing[A]] = findRing(_.origIndex == idx)

    def toStr: String = toList.mkString(", ")

    private def removeMyself(): Unit = {
      val before = prev
      val after  = next
      before.next = after
      after.prev = before
    }

    def moveForwardBy(n: Long): Unit = {
      removeMyself()
      @tailrec
      def loop(curr: MutableRing[A], n: Int): Unit =
        if (n == 0) {
          val after = curr.next
          self.prev = curr
          self.next = after
          curr.next = self
          after.prev = self
        } else loop(curr.next, n - 1)
      loop(prev, (n % (size - 1)).toInt)
    }

    def moveBackwardBy(n: Long): Unit = {
      removeMyself()
      @tailrec
      def loop(curr: MutableRing[A], n: Int): Unit =
        if (n == 0) {
          val before = curr.prev
          self.prev = before
          self.next = curr
          before.next = self
          curr.prev = self
        } else loop(curr.prev, n - 1)
      loop(next, (n % (size - 1)).toInt)
    }

    @tailrec
    final def valueAtNAfter(n: Int): A =
      if (n <= 0) value
      else next.valueAtNAfter(n - 1)
  }

  private object MutableRing {
    def apply[A](is: Seq[A]): MutableRing[A] = {
      assert(is.nonEmpty)
      val size = is.size

      val nodes = is.zipWithIndex
        .foldLeft(Option.empty[MutableRing[A]], Chunk.empty[MutableRing[A]]) { case ((prev, acc), (thisI, idx)) =>
          val mir = new MutableRing(thisI, idx, size, null, null)
          prev.foreach { prev =>
            mir.prev = prev
            prev.next = mir
          }
          Some(mir) -> (acc :+ mir)
        }
        ._2
      nodes.head.prev = nodes.last
      nodes.last.next = nodes.head
      nodes.head
    }
  }

  private implicit class MutableLongRing(private val mr: MutableRing[Long]) extends AnyVal {
    def moveByValue(): Unit =
      if (mr.value > 0) mr.moveForwardBy(mr.value)
      else if (mr.value < 0) mr.moveBackwardBy(math.abs(mr.value))
  }

  private def app(decryptionKey: Int, numMixes: Int)(dataFile: String) =
    resourceLines(dataFile).filterNot(_.isEmpty).map(_.toLong).map(_ * decryptionKey).runCollect.map { cs =>
      val mr = MutableRing(cs)
      (1 to numMixes).foreach { _ =>
        cs.indices.foreach { idx =>
          mr.findByOrigIndex(idx).foreach(_.moveByValue())
        }
      }
      val zero = mr.find(0).get
      (1000 to 3000 by 1000).map(zero.valueAtNAfter).sum
    }

  override def part1TestExpectation: Any           = 3
  override def part1Expectation: Any               = 1_087
  override def part1(dataFile: String): STask[Any] = app(1, 1)(dataFile)

  override def part2TestExpectation: Any           = 1623178306L
  override def part2Expectation: Any               = 13_084_440_324_666L
  override def part2(dataFile: String): STask[Any] = app(811589153, 10)(dataFile)
}
