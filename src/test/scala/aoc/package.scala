import zio._
import zio.stream._

package object aoc {
  type STask[A] = IO[String, A]

  def attempt[A](code: => A)(implicit trace: Trace): STask[A] = ZIO.attempt(code).mapError(_.getMessage)

  def resourceLines(resource: String): Stream[String, String] =
    (ZStream.fromResource(resource) >>> ZPipeline.utf8Decode >>> ZPipeline.splitLines).mapError(_.getMessage)
}
