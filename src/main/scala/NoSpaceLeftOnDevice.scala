import utils.Common.readString

import scala.annotation.tailrec

object NoSpaceLeftOnDevice {
    def main(args: Array[String]): Unit = {
        val input = readString("no-space-left-on-device.txt")
        val commands = getCommands(input).map(toCommand)
        val root = exploreFileSystem(commands.tail)
        println(allDirectories(List(root)).map(_.size).filter(_ <= 100000).sum)
        println(allDirectories(List(root)).map(_.size).filter(70_000_000 - root.size + _ >= 30_000_000).min)
    }

    sealed trait FileSystem {
        def size: Int = this match {
            case Directory(_, elements) => elements.map(_.size).sum
            case File(s) => s
        }
    }
    case class Directory(name: String, elements: Set[FileSystem] = Set()) extends FileSystem
    case class File(s: Int) extends FileSystem

    sealed trait Command
    case class CD(directory: String) extends Command
    case class LS(output: Set[FileSystem]) extends Command

    def getCommands(s: String): List[List[String]] =
        s.split(raw"""\$$""").map(getOutputLines).toList.tail

    def getOutputLines(s: String): List[String] = s.split("\n").map(_.strip).toList

    def toCommand(c: List[String]): Command = c.head match {
        case h if h.split(" ")(0) == "cd" => CD(h.split(" ")(1))
        case _ => LS(c.tail.map(toFileSystem).toSet)
    }

    def toFileSystem(s: String): FileSystem = s.split(" ")(0) match {
        case "dir" => Directory(s.split(" ")(1))
        case n => File(n.toInt)
    }

    @tailrec
    def exploreFileSystem(c: List[Command], d: Directory = Directory("/"), path: List[String] = List()): Directory = c match {
        case h :: t => h match {
            case CD(directory) => directory match {
                case ".." => exploreFileSystem(t, d, path.dropRight(1))
                case dir => exploreFileSystem(t, d, path :+ dir)
            }
            case LS(output) => exploreFileSystem(t, changeDirectoryContent(d, path, output), path)
        }
        case _ => d
    }

    def changeDirectoryContent(d: Directory, path: List[String], content: Set[FileSystem]): Directory = path match {
        case h :: t => Directory(d.name, d.elements.collect {
            case Directory(`h`, elems) => changeDirectoryContent(Directory(h, elems), t, content)
            case f => f
        })
        case _ => Directory(d.name, content)
    }

    def allDirectories(d: List[Directory]): List[Directory] =
        d :++ d.flatMap(e => allDirectories(e.elements.toList.collect {
            case e: Directory => e
        }))
}
