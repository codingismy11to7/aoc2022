import zio._
import zio.stream._

package object aoc {
  type STask[A] = IO[String, A]

  def attempt[A](code: => A)(implicit trace: Trace): STask[A] = ZIO.attempt(code).mapError(_.getMessage)

  def resourceBytes(resource: String): Stream[String, Byte] =
    ZStream.fromResource(resource).mapError(_.getMessage)

  private def resourceStrs(resource: String) = resourceBytes(resource) >>> ZPipeline.utf8Decode.mapError(_.getMessage)

  def resourceChars(resource: String): Stream[String, Char] =
    (resourceBytes(resource) >>> ZPipeline.utf8Decode.mapError(_.getMessage)).mapConcat(_.toCharArray)

  def resourceLines(resource: String): Stream[String, String] =
    resourceStrs(resource) >>> ZPipeline.splitLines
}
