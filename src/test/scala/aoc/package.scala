import zio.stream._

package object aoc {
  def resourceLines(resource: String): Stream[String, String] =
    (ZStream.fromResource(resource) >>> ZPipeline.utf8Decode >>> ZPipeline.splitLines).mapError(_.getMessage)
}
