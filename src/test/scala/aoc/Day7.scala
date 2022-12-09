package aoc

import zio._

import scala.annotation.tailrec

object Day7 extends ZIOAppDefault {
  final val Test  = false
  final val Part1 = false

  private sealed trait Path { self =>
    def parts: Chunk[String] = self match {
      case Path.NonRoot(parts) => parts
      case _                   => Chunk.empty
    }
    def /(part: String): Path = Path.NonRoot(parts :+ part)
    def parent: Option[Path] = self match {
      case Path.Root           => None
      case Path.NonRoot(parts) => Some(Path.NonRoot(parts.dropRight(1)).normalized)
    }
    def unsafeParent: Path = parent.getOrElse(sys.error("Tried to get parent of /"))
  }
  private object Path {
    def fromParts(parts: String*): Path = parts.foldLeft(Root: Path)(_ / _)

    case object Root extends Path
    private final case class NonRoot(parts0: Chunk[String]) extends Path {
      def normalized: Path = if (parts.nonEmpty) this else Root
    }
  }

  private sealed trait FS {
    def name: Path
  }
  private object FS {
    final case class Directory(name: Path)        extends FS
    final case class File(name: Path, size: Long) extends FS
  }

  private sealed trait DirTree {
    def isDirectory: Boolean
    def sizeOfFileOrContainedFiles: Long
    def fold[Z](z: Z)(f: (Z, DirTree) => Z): Z
    def foldFS[Z](
        z: Z,
    )(file: (Z, FS.File) => Z = (z: Z, _: FS.File) => z, dir: (Z, FS.Directory) => Z = (z: Z, _: FS.Directory) => z): Z
  }
  private object DirTree {
    private final case class TreeLeaf(file: FS.File) extends DirTree { self =>
      override def isDirectory: Boolean                                                     = false
      override def sizeOfFileOrContainedFiles: Long                                         = file.size
      override def fold[Z](z: Z)(f: (Z, DirTree) => Z): Z                                   = f(z, self)
      override def foldFS[Z](z: Z)(file: (Z, FS.File) => Z, dir: (Z, FS.Directory) => Z): Z = file(z, self.file)
    }

    private final case class TreeNode(dir: FS.Directory, childNodes: collection.mutable.Map[String, DirTree])
        extends DirTree { self =>
      override def isDirectory: Boolean                  = true
      override lazy val sizeOfFileOrContainedFiles: Long = foldFS(0L)(_ + _.size)

      override def fold[Z](z: Z)(f: (Z, DirTree) => Z): Z = {
        val afterDir = f(z, self)
        childNodes.values.foldLeft(afterDir)((z, tree) => tree.fold(z)(f))
      }

      override def foldFS[Z](z: Z)(file: (Z, FS.File) => Z, dir: (Z, FS.Directory) => Z): Z = {
        val afterSelf = dir(z, self.dir)
        childNodes.values.foldLeft(afterSelf)((z, tree) => tree.foldFS(z)(file, dir))
      }
    }

    def build(fsEntries: Chunk[FS]): DirTree = {
      def leaf(f: FS.File)    = TreeLeaf(f)
      def node(dirName: Path) = TreeNode(FS.Directory(dirName), collection.mutable.Map.empty)
      def tree(f: FS) = f match {
        case f: FS.File      => leaf(f)
        case d: FS.Directory => node(d.name)
      }
      val root = node(Path.Root)
      def addToTree(fs: FS): Unit = {
        @tailrec
        def loop(currPath: Path, currNode: TreeNode, rem: List[String]): Unit = rem match {
          case Nil          =>
          case fname :: Nil => currNode.childNodes += (fname -> tree(fs))
          case dirname :: rest =>
            val fullPath = currPath / dirname
            val newNode = currNode.childNodes
              .get(dirname)
              .collect { case tn: TreeNode => tn }
              .getOrElse(node(fullPath))
            currNode.childNodes += (dirname -> newNode)
            loop(fullPath, newNode, rest)
        }
        loop(Path.Root, root, fs.name.parts.toList)
      }
      fsEntries.foreach(addToTree)
      root
    }
  }

  private final case class State(cwd: Path = Path.Root, fsEntries: Chunk[FS] = Chunk.empty) {
    def modCwd(f: Path => Path): State = copy(f(cwd))
    def withEntry(newEntry: FS): State = copy(fsEntries = fsEntries :+ newEntry)

    lazy val dirTree: DirTree = DirTree.build(fsEntries)
  }

  private val data = resourceLines(s"7/${if (Test) "test" else "input"}.txt")

  private val directoryTree = {
    val cmd       = """^\$ (.+)$""".r
    val cdDir     = """^cd (.+)$""".r
    val dirEntry  = """^dir (.+)$""".r
    val fileEntry = """^(\d+) (.+)$""".r
    data
      .runFold(State()) {
        case (state, cmd(subcmd)) =>
          subcmd match {
            case "cd /"       => state.modCwd(_ => Path.Root)
            case "cd .."     => state.modCwd(_.unsafeParent)
            case cdDir(dirName) => state.modCwd(_ / dirName)
            case "ls"      => state
            case _              => sys.error(s"Invalid command $subcmd")
          }
        case (state, dirEntry(dirName))      => state.withEntry(FS.Directory(state.cwd / dirName))
        case (state, fileEntry(size, fname)) => state.withEntry(FS.File(state.cwd / fname, size.toLong))
        case (_, line)                       => sys.error(s"Invalid line $line")
      }
      .map(_.dirTree)
  }

  private lazy val part1 = directoryTree
    .map(_.fold(0L) {
      case (acc, tree) if tree.isDirectory && tree.sizeOfFileOrContainedFiles <= 100_000 =>
        acc + tree.sizeOfFileOrContainedFiles

      case (acc, _) => acc
    })

  private lazy val part2 = directoryTree.map { dirTree =>
    val totalSpace  = 70_000_000
    val neededSpace = 30_000_000
    val usedSpace   = dirTree.sizeOfFileOrContainedFiles
    val needToFree  = neededSpace - (totalSpace - usedSpace)

    dirTree.fold(Option.empty[Long]) {
      case (acc, tree) if tree.isDirectory && tree.sizeOfFileOrContainedFiles >= needToFree =>
        Some(math.min(tree.sizeOfFileOrContainedFiles, acc.getOrElse(Long.MaxValue)))
      case (acc, _) => acc
    }
  }

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    (if (Part1) part1 else part2).flatMap(ZIO.debug(_))
}
